module Hix.Test.NewTest where

import Exon (exon)
import Hedgehog (evalEither, (===))
import Path (absdir, relfile)

import qualified Hix.Data.NewProjectConfig
import Hix.Data.NewProjectConfig (HixUrl, NewProjectConfig (NewProjectConfig))
import Hix.Data.ProjectFile (ProjectFile (ProjectFile))
import Hix.Monad (runM)
import Hix.New (license, newProjectFiles)
import Hix.Test.Utils (UnitTest)

conf :: NewProjectConfig
conf =
  NewProjectConfig {name = "spider", packages = False, hixUrl = def, author = "Me"}

flake :: Text
flake =
  [exon|{
  description = "A Haskell project";

  inputs.hix.url = "#{(def :: HixUrl).unHixUrl}";

  outputs = {hix, ...}: hix.lib.flake {
    hackage.versionFile = "ops/version.nix";

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Me";
      ghc-options = ["-Wall"];
    };

    packages.spider = {
      src = ./.;
      cabal.meta.synopsis = "A Haskell project";

      library = {
        enable = true;
        dependencies = [
          "containers"
        ];
      };

      executable.enable = true;

      test = {
        enable = true;
        dependencies = [
          "hedgehog >= 1.1 && < 1.3"
          "tasty ^>= 1.4"
          "tasty-hedgehog >= 1.3 && < 1.5"
        ];
      };

    };
  };
}
|]

libModule :: Text
libModule =
  [exon|module Spider where

name :: String
name = "spider"
|]

appMainModule :: Text
appMainModule =
  [exon|module Main where

import Spider (name)

main :: IO ()
main = putStrLn ("Hello " <> name)
|]

testMainModule :: Text
testMainModule =
  [exon|module Main where

import Hedgehog (property, test, withTests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Spider.Test.NameTest (test_name)

tests :: TestTree
tests =
  testGroup "all" [
    testProperty "name" (withTests 1 (property (test test_name)))
  ]

main :: IO ()
main = defaultMain tests
|]

nameTestModule :: Text
nameTestModule =
  [exon|module Spider.Test.NameTest where

import Hedgehog (TestT, (===))

import Spider (name)

test_name :: TestT IO ()
test_name = "spider" === name
|]

target :: [ProjectFile]
target =
  [
    ProjectFile [relfile|flake.nix|] flake,
    ProjectFile [relfile|LICENSE|] (license "Me"),
    ProjectFile [relfile|ops/version.nix|] [exon|"0.1.0.0"|],
    ProjectFile [relfile|lib/Spider.hs|] libModule,
    ProjectFile [relfile|app/Main.hs|] appMainModule,
    ProjectFile [relfile|test/Main.hs|] testMainModule,
    ProjectFile [relfile|test/Spider/Test/NameTest.hs|] nameTestModule
  ]

test_new :: UnitTest
test_new = do
  res <- evalEither =<< liftIO (runM [absdir|/project|] (newProjectFiles conf))
  target === res
