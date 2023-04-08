module Hix.Component where

import Control.Monad.Trans.Reader (ask)
import Data.List.Extra (firstJust)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Text as Text
import Exon (exon)
import Path (Abs, Dir, File, Path, Rel, SomeBase (Abs, Rel), isProperPrefixOf, stripProperPrefix)

import qualified Hix.Data.ComponentConfig
import Hix.Data.ComponentConfig (
  ComponentConfig,
  PackageConfig (PackageConfig),
  PackageName (PackageName),
  PackagesConfig,
  SourceDir (SourceDir),
  Target (Target),
  )
import Hix.Data.Error (pathText)
import qualified Hix.Monad as Monad
import Hix.Monad (Env (Env), M, noteEnv)
import qualified Hix.Options as Options
import Hix.Options (
  ComponentCoords,
  ComponentSpec (ComponentSpec),
  PackageSpec (PackageSpec),
  TargetSpec (TargetForComponent, TargetForFile),
  )

tryPackageByDir ::
  PackagesConfig ->
  Path Rel Dir ->
  Maybe PackageConfig
tryPackageByDir config dir =
  find match (Map.elems config)
  where
    match pkg = pkg.src == dir

packageByDir ::
  PackagesConfig ->
  Path Rel Dir ->
  M PackageConfig
packageByDir config dir =
  noteEnv [exon|No package at this directory: #{pathText dir}|] (tryPackageByDir config dir)

packageForSpec ::
  PackagesConfig ->
  PackageSpec ->
  M PackageConfig
packageForSpec config = \case
  PackageSpec _ (Just (Abs dir)) -> do
    Env {root} <- ask
    rel <- noteEnv [exon|Path is not a subdirectory of the project root: #{pathText dir}|] (stripProperPrefix root dir)
    packageByDir config rel
  PackageSpec (PackageName name) (Just (Rel dir)) | Text.elem '/' name ->
    packageByDir config dir
  PackageSpec name dir ->
    noteEnv [exon|No package matching '##{name}'|] (config !? name <|> (tryDir =<< dir))
    where
      tryDir = \case
        Abs _ -> Nothing
        Rel rd -> tryPackageByDir config rd

matchComponent :: ComponentConfig -> ComponentSpec -> Bool
matchComponent candidate (ComponentSpec name dir) =
  candidate.name == name || any (\ d -> elem @[] d (coerce candidate.sourceDirs)) dir

componentError :: PackageName -> ComponentSpec -> Text
componentError pname spec =
  [exon|No component with name or source dir '##{name}' in the package '##{pname}'|]
  where
    name = spec.name

targetForComponent ::
  PackagesConfig ->
  ComponentCoords ->
  M Target
targetForComponent config spec = do
  package <- packageForSpec config spec.package
  component <- noteEnv (componentError package.name spec.component) (find match (Map.elems package.components))
  pure Target {sourceDir = Nothing, ..}
  where
    match = flip matchComponent spec.component

targetForFile ::
  PackagesConfig ->
  Path Abs File ->
  M Target
targetForFile config file = do
  Env {root} <- ask
  fileRel <- stripProperPrefix root file
  (package, subpath) <- pkgError (firstJust (matchPackage fileRel) (Map.elems config))
  (component, sourceDir) <- compError (firstJust (matchSourceDir subpath) (Map.elems package.components))
  pure Target {..}
  where
    matchPackage fileRel package@PackageConfig {src} = do
      subpath <- stripProperPrefix src fileRel
      pure (package, subpath)
    matchSourceDir subpath component = do
      let match d@(SourceDir dir) = if isProperPrefixOf dir subpath then Just d else Nothing
      dir <- firstJust match (coerce component.sourceDirs)
      pure (component, Just dir)
    pkgError = noteEnv "No package contains this file"
    compError = noteEnv "No component source dir contains this file"

targetComponent ::
  PackagesConfig ->
  TargetSpec ->
  M Target
targetComponent config = \case
  TargetForComponent spec ->
    targetForComponent config spec
  TargetForFile spec ->
    targetForFile config spec
