module Hix.Integration.Hackage where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Monad.Catch (bracket)
import Data.Text.IO (writeFile)
import qualified Distribution.Server as Server
import Distribution.Server (ListenOn (..), ServerConfig (..))
import Exon (exon)
import Network.URI (URI (..), URIAuth (URIAuth), nullURI)
import Path (Abs, Dir, File, Path, reldir, toFilePath, (</>))
import Path.IO (createDirIfMissing)
import System.Environment (lookupEnv)

import Hix.Data.Monad (M)
import Hix.Error (pathText)
import Hix.Integration.Data.Options (HackageServeOptions (..))
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Resources (cabalVerbosity)
import Hix.Monad (catchIOM, noteFatal, withTempDir)
import Hix.Network (Port (..), freePort)

withHackageOnPort ::
  Path Abs Dir ->
  Port ->
  M a ->
  M a
withHackageOnPort root port ma = do
  confStaticDir <- noteFatal "$hackage_data_dir unset" =<< liftIO (lookupEnv "hackage_data_dir")
  createDirIfMissing True tmpDir
  defaults <- liftIO $ Server.defaultServerConfig
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
  Path Abs Dir ->
  (Port -> M a) ->
  M a
withHackage root use = do
  port <- freePort
  withHackageOnPort root port (use port)

writePortFile :: Port -> Path Abs File -> M ()
writePortFile port path = do
  Log.verbose [exon|Writing port number #{show port.value} to #{pathText path}|]
  catchIOM (writeFile (toFilePath path) (show port.value)) \ err ->
    Log.warn [exon|Couldn't write port to #{pathText path}: #{err}|]

hackageServe :: HackageServeOptions -> M ()
hackageServe HackageServeOptions {portFile} = do
  withTempDir "hackage-serve" \ tmp ->
    withHackage tmp \ port -> do
      traverse_ (writePortFile port) portFile
      forever (liftIO (threadDelay 1_000_000))
