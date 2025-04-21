module Hix.Integration.Hackage where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Monad.Catch (bracket)
import Data.Aeson (Value)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.IO (writeFile)
import qualified Distribution.Server as Server
import Distribution.Server (ListenOn (..), ServerConfig (..))
import Exon (exon)
import Network.URI (URI (..), URIAuth (URIAuth), nullURI)
import Path (Abs, Dir, File, Path, parseRelDir, parseRelFile, reldir, toFilePath, (</>))
import Path.IO (createDir, createDirIfMissing)
import System.Environment (lookupEnv)
import Text.Casing (pascal)

import Hix.Data.ComponentConfig (ModuleName (..))
import Hix.Data.Dep (Dep)
import Hix.Data.Monad (AppResources (..), M, appRes)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (PackageName)
import Hix.Error (pathText)
import Hix.Hackage (hackagePostQuery, hackagePut)
import Hix.Http (httpManager)
import Hix.Integration.Data.Options (HackageServeOptions (..))
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Config (validateContextRepo)
import Hix.Managed.Cabal.Data.ContextHackageRepo (
  ContextHackageLocation (..),
  ContextHackagePassword (..),
  ContextHackageRepo (..),
  contextHackageRepo,
  )
import Hix.Managed.Cabal.Data.HackageLocation (HackageLocation (..), HackageTls (..))
import Hix.Managed.Cabal.Data.HackageRepo (HackageRepo (..))
import qualified Hix.Managed.Cabal.Init as Cabal
import Hix.Managed.Cabal.Init (remoteRepo)
import Hix.Managed.Cabal.Resources (cabalVerbosity)
import Hix.Managed.Cabal.Sdist (sourceDistribution)
import Hix.Managed.Cabal.Upload (UploadConfig (..), publishPackage)
import Hix.Managed.Handlers.HackageClient (HackageClient, HackageResponse (..))
import qualified Hix.Managed.Handlers.HackageClient.Prod as HackageResources
import qualified Hix.Managed.Handlers.HackageClient.Prod as HackageClient
import Hix.Managed.Handlers.HackageClient.Prod (HackageResources (..), mockUser)
import Hix.Monad (appContextDebug, catchIOM, noteFatal, tryIOM, withTempDir)
import Hix.Network (Port (..), freePort)
import Hix.Path (resolvePathSpec)
import Hix.Pretty (showP)

withHackageOnPort ::
  Path Abs Dir ->
  Port ->
  M a ->
  M a
withHackageOnPort root port ma = do
  confStaticDir <- noteFatal "$hackage_data_dir unset" =<< liftIO (lookupEnv "hackage_data_dir")
  createDirIfMissing True tmpDir
  defaults <- liftIO Server.defaultServerConfig
  confVerbosity <- cabalVerbosity
  let
    config =
      defaults {
        confStateDir,
        confStaticDir,
        confTmpDir = toFilePath tmpDir,
        confVerbosity,
        confListenOn = ListenOn {loPortNum = fromIntegral port, loIP = "0.0.0.0"},
        confHostUri = nullURI {
          uriScheme = "http:",
          uriAuthority = Just (URIAuth "" "localhost" (':' : show port.value))
        }
      }
  bracket (liftIO (acquire config)) release \ (_, _) -> ma
  where
    acquire config = do
      server <- Server.initialise config
      Server.initState server ("admin", "admin")
      handle <- async $ Server.run server
      pure (server, handle)

    release (server, handle) =
      liftIO do
        Server.shutdown server
        cancel handle

    confStateDir = toFilePath stateDir
    tmpDir = stateDir </> [reldir|tmp|]
    stateDir = root </> [reldir|state|]

withHackage ::
  (Port -> M a) ->
  M a
withHackage use =
  withTempDir "hackage" \ root -> do
    port <- freePort
    withHackageOnPort root port (use port)

testRepo :: Port -> HackageRepo
testRepo port =
  HackageRepo {
    name = "test",
    description = "test",
    location = HackageLocation {
      host = "localhost",
      port = Just (fromIntegral port.value),
      auth = Just ("test", "test"),
      tls = TlsOff
    },
    enable = True,
    secure = Nothing,
    indexState = Nothing,
    solver = True,
    publish = True,
    keys = Nothing
  }

data TestHackageRepo =
  TestHackageRepo {
    context :: ContextHackageRepo,
    repo :: HackageRepo
  }
  deriving stock (Eq, Show)

testContextRepo :: Port -> ContextHackageRepo
testContextRepo port =
  (contextHackageRepo "test") {
    description = Just "test",
    location = Just (ContextHackageLocation [exon|http://localhost:#{show port.value}|]),
    user = Just "test",
    password = Just (PasswordUnobscured "test"),
    solver = Just True,
    publish = Just True
  }

withHackageRepo ::
  (TestHackageRepo -> M a) ->
  M a
withHackageRepo use =
  withHackage \ port -> do
    let context = testContextRepo port
    repo <- validateContextRepo context
    use TestHackageRepo {..}

data TestHackage =
  TestHackage {
    context :: ContextHackageRepo,
    repo :: HackageRepo,
    client :: HackageClient
  }

testClient :: HackageLocation -> M HackageClient
testClient location = do
  manager <- httpManager
  let
    res = HackageResources {
      manager,
      description = "mock Hackage",
      location
    }

    adminRes = res {HackageResources.location = location {auth = Just ("admin", "admin")}}

    adminClient = HackageClient.handlersProd adminRes

  _ <- hackagePostQuery Right adminClient "users/" mockUser HackageNoResponse
  _ <- hackagePut @Value Right adminClient "packages/uploaders/user/test"
  pure (HackageClient.handlersProd res)

withHackageClient ::
  (TestHackage -> M a) ->
  M a
withHackageClient use =
  withHackageRepo \ TestHackageRepo {..} -> do
    client <- testClient repo.location
    use TestHackage {..}

data HackageId =
  HackageId {
    package :: PackageId,
    modules :: [(ModuleName, Text)],
    deps :: [Dep]
  }
  deriving stock (Eq, Show)

defaultModule :: PackageName -> (ModuleName, Text)
defaultModule name =
  (ModuleName moduleName, [exon|module #{moduleName} where|])
  where
    moduleName = toText (pascal (toString name))

testCabalContent :: PackageId -> [ModuleName] -> [Dep] -> Text
testCabalContent PackageId {..} modules deps =
  [exon|cabal-version: 1.12
name: ##{name}
version: #{showP version}
license: MIT
maintainer: hix
synopsis: Synopsis
description: A test package named ##{name}
category: Test
build-type: Simple
library
    exposed-modules:  #{Text.intercalate ", " (coerce modules)}
    hs-source-dirs:   .
    default-language: GHC2021
    build-depends: #{Text.intercalate ", " depIds}
|]
  where
    depIds = "base <5" : (showP <$> deps)

createPackageDir ::
  Path Abs Dir ->
  HackageId ->
  M (Path Abs Dir)
createPackageDir root HackageId {package, deps, modules = modulesSpec} = do
  tryIOM do
    dir <- parseRelDir (toString package.name)
    let packageDir = root </> dir
    createDir packageDir
    for_ modules \ (moduleName, content) -> do
      fileName <- parseRelFile [exon|##{moduleName}.hs|]
      Text.writeFile (toFilePath (packageDir </> fileName)) content
    cabalName <- parseRelFile [exon|#{toString package.name}.cabal|]
    Text.writeFile (toFilePath (packageDir </> cabalName)) (testCabalContent package (fst <$> modules) deps)
    pure packageDir
  where
    modules = toList (fromMaybe [defaultModule package.name] (nonEmpty modulesSpec))

withHackageIds ::
  [HackageId] ->
  (TestHackage -> M a) ->
  M a
withHackageIds ids use =
  withHackageClient \ hackage -> do
    publish hackage.repo
    use hackage
  where
    publish repo = do
      flags <- Cabal.solveFlags [remoteRepo repo] def
      verbosity <- cabalVerbosity
      tmp <- appRes.tmp
      let uploadConfig = UploadConfig {verbosity, user = "test", password = "test"}
          root = tmp </> [reldir|hackage-ids|]
      createDir root
      for_ ids \ hid@HackageId {package} ->
        appContextDebug [exon|publishing test package #{showP package}|] do
          packageDir <- createPackageDir root hid
          targz <- sourceDistribution packageDir package
          publishPackage uploadConfig flags targz

writePortFile :: Port -> Path Abs File -> M ()
writePortFile port path = do
  Log.verbose [exon|Writing port number #{show port.value} to #{pathText path}|]
  catchIOM (writeFile (toFilePath path) (show port.value)) \ err ->
    Log.warn [exon|Couldn't write port to #{pathText path}: #{err}|]

hackageServe :: HackageServeOptions -> M ()
hackageServe HackageServeOptions {portFile = portFileSpec} = do
  portFile <- traverse resolvePathSpec portFileSpec
  withHackage \ port -> do
    traverse_ (writePortFile port) portFile
    forever (liftIO (threadDelay 1_000_000))
