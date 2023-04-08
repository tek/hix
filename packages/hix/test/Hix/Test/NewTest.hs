module Hix.Test.NewTest where

import Exon (exon)
import Hedgehog (TestT, evalEither, (===))
import Path (absdir, relfile)

import qualified Hix.Data.NewProjectConfig
import Hix.Data.NewProjectConfig (HixUrl, NewProjectConfig (NewProjectConfig))
import Hix.Monad (runM)
import Hix.New (ProjectFile (ProjectFile), newProjectFiles, license)

conf :: NewProjectConfig
conf =
  NewProjectConfig {name = "spider", packages = False, hixUrl = def, author = "Me"}

flake :: Text
flake =
  [exon|{
  description = "A Haskell project";

  inputs.hix.url = "#{(def :: HixUrl).unHixUrl}";

  outputs = {hix, ...}: hix.lib.flake {
    packages.spider = {
      src = ./.;
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

testMainModule :: Text
testMainModule =
  [exon|module Main where

import Hedgehog (TestT, property, test, withTests)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Spider.Test.NameTest (test_name)

tests :: TestTree
tests =
  testGroup "all" [
      testProperty "name" (withTests 1 (property (test test_name)))
    ]
  ]

main :: IO ()
main =
  defaultMain tests
|]

nameTestModule :: Text
nameTestModule =
  [exon|module Spider.Test.NameTest where

import Hedgehog (TestT, (===))

import Spider (name)

test_name :: TestT IO ()
test_name =
  "spider" === name
|]

target :: [ProjectFile]
target =
  [
    ProjectFile [relfile|flake.nix|] flake,
    ProjectFile [relfile|LICENSE|] (license "Me"),
    ProjectFile [relfile|lib/Spider.hs|] libModule,
    ProjectFile [relfile|test/Main.hs|] testMainModule,
    ProjectFile [relfile|test/Spider/Test/NameTest.hs|] nameTestModule
  ]

test_new :: TestT IO ()
test_new = do
  res <- evalEither =<< liftIO (runM [absdir|/project|] (newProjectFiles conf))
  target === res
