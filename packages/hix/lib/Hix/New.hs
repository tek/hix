module Hix.New where

import Exon (exon)
import Path (
  dirname,
  fromRelDir,
  parseRelDir,
  parseRelFile,
  reldir,
  relfile,
  (</>),
  )
import qualified Data.Text as Text
import Text.Casing (pascal)

import Hix.Bootstrap (initGitAndFlake)
import qualified Hix.Console as Console
import qualified Hix.Data.NewProjectConfig
import Hix.Data.NewProjectConfig (
  Author,
  HixUrl (HixUrl),
  InitProjectConfig (InitProjectConfig),
  NewProjectConfigCommon (NewProjectConfigCommon),
  ProjectName (ProjectName), NewProjectConfig,
  )
import qualified Hix.Data.ProjectFile
import Hix.Data.ProjectFile (ProjectFile (ProjectFile), createFile)
import Hix.Monad (M, noteEnv, local)
import Hix.Data.Monad (AppResources (cwd))
import Hix.Error (pathText)

license :: Author -> Text
license author =
  [exon|Copyright (c) 2023 ##{author}

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following
  disclaimer.
  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
  disclaimer in the documentation and/or other materials provided with the distribution.

Subject to the terms and conditions of this license, each copyright holder and contributor hereby grants to those
receiving rights under this license a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable (except
for failure to satisfy the conditions of this license) patent license to make, have made, use, offer to sell, sell,
import, and otherwise transfer this software, where such license applies only to those patent claims, already acquired
or hereafter acquired, licensable by such copyright holder or contributor that are necessarily infringed by:

  (a) their Contribution(s) (the licensed copyrights of copyright holders and non-copyrightable additions of
  contributors, in source or binary form) alone; or
  (b) combination of their Contribution(s) with the work of authorship to which such Contribution(s) was added by such
  copyright holder or contributor, if, at the time the Contribution is added, such addition causes such combination to
  be necessarily infringed. The patent license shall not apply to any other combinations which include the Contribution.

Except as expressly stated above, no rights or licenses from any copyright holder or contributor is granted under this
license, whether expressly, by implication, estoppel or otherwise.

DISCLAIMER

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|]

flake :: InitProjectConfig -> Text
flake InitProjectConfig {
  name = ProjectName name,
  config = NewProjectConfigCommon { hixUrl = HixUrl url, ..}
} =
  [exon|{
  description = "A Haskell project";

  inputs.hix.url = "#{url}";

  outputs = {hix, ...}: hix.lib.flake {
    hackage.versionFile = "ops/version.nix";

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "##{author}";
      ghc-options = ["-Wall"];
    };

    packages.#{name} = {
      src = ./#{src};
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
          "hedgehog >= 1.1 && < 1.5"
          "tasty ^>= 1.4"
          "tasty-hedgehog >= 1.3 && < 1.5"
        ];
      };

    };
  };
}
|]
  where
    src | packages = [exon|packages/#{name}|]
        | otherwise = "."

libModule :: InitProjectConfig -> Text -> Text
libModule conf modName =
  [exon|module #{modName} where

name :: String
name = "#{conf.name.unProjectName}"
|]

appMainModule :: Text -> Text
appMainModule modName =
  [exon|module Main where

import #{modName} (name)

main :: IO ()
main = putStrLn ("Hello " <> name)
|]

testMainModule :: Text -> Text
testMainModule modName =
  [exon|module Main where

import Hedgehog (property, test, withTests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import #{modName}.Test.NameTest (test_name)

tests :: TestTree
tests =
  testGroup "all" [
    testProperty "name" (withTests 1 (property (test test_name)))
  ]

main :: IO ()
main = defaultMain tests
|]

nameTestModule :: InitProjectConfig -> Text -> Text
nameTestModule conf modName =
  [exon|module #{modName}.Test.NameTest where

import Hedgehog (TestT, (===))

import #{modName} (name)

test_name :: TestT IO ()
test_name = "#{conf.name.unProjectName}" === name
|]

newProjectFiles :: InitProjectConfig -> M [ProjectFile]
newProjectFiles conf = do
  nameDir <- pathError (parseRelDir modNameS)
  let packageDir = if conf.config.packages then [reldir|packages|] </> nameDir else [reldir|.|]
  libFile <- pathError (parseRelFile [exon|#{modNameS}.hs|])
  pure [
    ProjectFile {path = [relfile|flake.nix|], content = flake conf},
    ProjectFile {path = packageDir </> [relfile|LICENSE|], content = license conf.config.author},
    ProjectFile {path = packageDir </> [relfile|ops/version.nix|], content = [exon|"0.1.0.0"|]},
    ProjectFile {path = packageDir </> [reldir|lib|] </> libFile, content = libModule conf modName},
    ProjectFile {path = packageDir </> [relfile|app/Main.hs|], content = appMainModule modName},
    ProjectFile {path = packageDir </> [relfile|test/Main.hs|], content = testMainModule modName},
    ProjectFile {
      path = packageDir </> [reldir|test|] </> nameDir </> [relfile|Test/NameTest.hs|],
      content = nameTestModule conf modName
    }
    ]
  where
    modName = toText modNameS
    modNameS = pascal (toString conf.name.unProjectName)

initProject :: InitProjectConfig -> M ()
initProject conf = do
  traverse_ createFile =<< newProjectFiles conf
  unless conf.config.noInitGitAndFlake initGitAndFlake

newProject :: NewProjectConfig -> M ()
newProject conf = do
  let name =
        fromMaybe
          (ProjectName . Text.dropWhileEnd (== '/') . Text.pack . fromRelDir . dirname $ conf.directory)
          conf.name
  when conf.printDirectory $ Console.out (pathText conf.directory)
  local (\res -> res { cwd = conf.directory }) $
    initProject $ InitProjectConfig {name = name, config = conf.config}

pathError :: Maybe a -> M a
pathError = noteEnv "Can't convert project name to file path"
