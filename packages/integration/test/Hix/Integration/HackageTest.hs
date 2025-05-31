module Hix.Integration.HackageTest where

import Data.Aeson (Value)
import Distribution.Verbosity (Verbosity, silent)
import Distribution.Version (mkVersion)
import Exon (exon)
import Path (Abs, Dir, File, Path, Rel, reldir, relfile, (</>))
import Path.IO (createDirIfMissing)

import Hix.Data.Monad (AppResources (..), M, appRes)
import Hix.Data.PackageId (PackageId (..))
import Hix.Hackage (hackagePostQuery, hackagePut)
import Hix.Http (httpManager)
import Hix.Integration.Hackage (withHackage)
import Hix.Integration.Utils (UnitTest, addFile, runMTest)
import Hix.Managed.Cabal.Data.HackageLocation (HackageAuth (..), HackageLocation (..), HackageTls (TlsOff))
import Hix.Managed.Cabal.Data.HackageRepo (HackageRepo (secure))
import Hix.Managed.Cabal.HackageRepo (hackageRepo)
import qualified Hix.Managed.Cabal.Init as Cabal
import Hix.Managed.Cabal.Init (remoteRepo)
import Hix.Managed.Cabal.Sdist (sourceDistribution)
import Hix.Managed.Cabal.Upload (UploadConfig (..), publishPackage, publishRevision)
import Hix.Managed.Handlers.HackageClient (HackageResponse (HackageNoResponse))
import qualified Hix.Managed.Handlers.HackageClient.Prod as HackageClient
import Hix.Managed.Handlers.HackageClient.Prod (HackageResources (..))
import Hix.Network (Port (..))

testUser :: NonEmpty (Text, Text)
testUser =
  [
    ("username", "test"),
    ("password", "test"),
    ("repeat-password", "test")
  ]

verbosity :: Verbosity
verbosity = silent

initialCabalContents :: Text
initialCabalContents =
  [exon|cabal-version: 3.0
name: hix-test
version: 1
description: Test project for hackage uploads
license: MIT
build-type: Simple
library
  exposed-modules: Root
  hs-source-dirs: lib
  build-depends: aeson ==2.2.*, extra ==0.7.*, base <5
  default-language: Haskell2010
|]

revisedCabalContents :: Text
revisedCabalContents =
  [exon|cabal-version: 3.0
name: hix-test
version: 1
description: Test project for hackage uploads
license: MIT
build-type: Simple
library
  exposed-modules: Root
  hs-source-dirs: lib
  build-depends: aeson >=2.2 && <2.4, extra ==0.7.*, base <5
  default-language: Haskell2010
|]

libMod :: Text
libMod =
  [exon|module Root where

import Data.Aeson
import Data.List.Extra
string :: String
string = "hello"
|]

cabalName :: Path Rel File
cabalName = [relfile|hix-test.cabal|]

createProject ::
  Path Abs Dir ->
  M (Path Abs Dir)
createProject tmp = do
  let projectRoot = tmp </> [reldir|project|]
  createDirIfMissing True projectRoot
  addFile projectRoot cabalName initialCabalContents
  addFile projectRoot [relfile|lib/Root.hs|] libMod
  pure projectRoot

testServer ::
  Port ->
  M ()
testServer port = do
  manager <- httpManager
  let
    auth = (HackageAuthPassword {user = "admin", password = "admin"})
    res = HackageResources {
      description = "test",
      manager,
      location = HackageLocation {
        host = "localhost",
        port = Just (fromIntegral port),
        auth = Just auth,
        tls = TlsOff
      }
    }
    userRes = res {location = res.location {auth = Just (HackageAuthPassword {user = "test", password = "test"})}}
    client = HackageClient.handlersProd res
    userClient = HackageClient.handlersProd userRes
    repo = (hackageRepo "test" res.location) {secure = Nothing}
  _ <- hackagePostQuery Right client "users/" testUser HackageNoResponse
  _ <- hackagePut @Value Right client "packages/uploaders/user/test"
  let rrepo = remoteRepo repo
  flags <- Cabal.solveFlags [rrepo] def
  root <- appRes.root
  projectDir <- createProject root
  targz <- sourceDistribution projectDir package
  publishPackage uploadConfig flags targz
  addFile projectDir cabalName revisedCabalContents
  void $ publishRevision [userClient] package (projectDir </> cabalName)
  where
    package = PackageId {name = "hix-test", version = mkVersion [1]}
    uploadConfig = UploadConfig {verbosity, user = "test", password = "test"}

test_hackage :: UnitTest
test_hackage =
  runMTest False do
      withHackage testServer
