module Hix.Component where

import Data.List.Extra (firstJust)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Text as Text
import Exon (exon)
import Path (Abs, Dir, File, Path, Rel, SomeBase (Abs, Rel), isProperPrefixOf, reldir, stripProperPrefix)

import qualified Hix.Data.ComponentConfig
import Hix.Data.ComponentConfig (
  ComponentConfig,
  PackageConfig (PackageConfig),
  PackagesConfig,
  SourceDir (SourceDir),
  Target (Target),
  TargetOrDefault (DefaultTarget, ExplicitTarget, NoDefaultTarget),
  )
import qualified Hix.Data.Options as Options
import Hix.Data.Options (ComponentCoords, ComponentSpec (ComponentSpec), PackageSpec (PackageSpec), TargetSpec (..))
import Hix.Data.PackageName (PackageName (PackageName))
import Hix.Error (pathText)
import Hix.Monad (M, clientError, noteEnv)
import Hix.Path (PathSpecResolver (resolvePathSpec), rootDir)

data ResolvedPackage =
  ResolvedPackage Bool PackageConfig
  |
  NoPackage Text
  deriving stock (Eq, Show, Generic)

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

packageDefault :: Maybe PackageName -> PackagesConfig -> ResolvedPackage
packageDefault mainPkg = \case
  [] -> NoPackage "Project has no packages."
  [(_, pkg)] -> ResolvedPackage False pkg
  pkgs | Just name <- mainPkg ->
    case Map.lookup name pkgs of
      Just pkg ->
        ResolvedPackage False pkg
      Nothing ->
        NoPackage (
          [exon|Project has multiple packages and the main package '##{name}' is not among them. |] <>
          "Specify -p or -f to choose one explicitly."
        )
  _ -> NoPackage "Project has more than one package, specify -p or -f."

packageForSpec ::
  Path Abs Dir ->
  PackagesConfig ->
  PackageSpec ->
  M PackageConfig
packageForSpec root config = \case
  PackageSpec _ (Just (Abs dir)) -> do
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

packageForSpecOrDefault ::
  Path Abs Dir ->
  Maybe PackageName ->
  PackagesConfig ->
  Maybe PackageSpec ->
  M ResolvedPackage
packageForSpecOrDefault root mainPkg config = \case
  Just pkg -> ResolvedPackage True <$> packageForSpec root config pkg
  Nothing -> pure (packageDefault mainPkg config)

matchComponent :: ComponentConfig -> ComponentSpec -> Bool
matchComponent candidate (ComponentSpec name dir) =
  candidate.name == name || any (\ d -> elem @[] d (coerce candidate.sourceDirs)) dir

componentError :: PackageName -> ComponentSpec -> Text
componentError pname spec =
  [exon|No component with name or source dir '##{name}' in the package '##{pname}'|]
  where
    name = spec.name

undecidableComponentError :: PackageName -> Text
undecidableComponentError pname =
  [exon|Please specify a component name or source dir with -c for the package '##{pname}'|]

testComponent :: ComponentSpec
testComponent =
  ComponentSpec "test" (Just (SourceDir [reldir|test|]))

defaultComponent :: PackageConfig -> Either Text Target
defaultComponent package = do
  component <- maybeToRight (undecidableComponentError package.name) (selectComponent package.components)
  pure Target {sourceDir = Nothing, ..}
  where
    selectComponent [(_, comp)] = Just comp
    selectComponent (Map.elems -> comps) = find (match testComponent) comps
    match = flip matchComponent

targetInPackage ::
  ResolvedPackage ->
  Maybe ComponentSpec ->
  M TargetOrDefault
targetInPackage (ResolvedPackage _ package) (Just comp) = do
  component <- noteEnv (componentError package.name comp) (find match (Map.elems package.components))
  pure (ExplicitTarget (Target {sourceDir = Nothing, ..}))
  where
    match cand = matchComponent cand comp
targetInPackage (ResolvedPackage True package) Nothing =
  either clientError pure (ExplicitTarget <$> defaultComponent package)
targetInPackage (ResolvedPackage False package) Nothing = do
  either (pure . NoDefaultTarget) pure (DefaultTarget <$> defaultComponent package)
targetInPackage (NoPackage err) _ = pure (NoDefaultTarget err)

targetForComponent ::
  Path Abs Dir ->
  Maybe PackageName ->
  PackagesConfig ->
  ComponentCoords ->
  M TargetOrDefault
targetForComponent root mainPkg config spec = do
  package <- packageForSpecOrDefault root mainPkg config spec.package
  targetInPackage package spec.component

targetForFile ::
  Path Abs Dir ->
  PackagesConfig ->
  Path Abs File ->
  M Target
targetForFile root config file = do
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

targetComponentIn ::
  Path Abs Dir ->
  Maybe PackageName ->
  PackagesConfig ->
  TargetSpec ->
  M TargetOrDefault
targetComponentIn root mainPkg config = \case
  TargetForComponent spec ->
    targetForComponent root mainPkg config spec
  TargetForFile spec ->
    ExplicitTarget <$> (targetForFile root config =<< resolvePathSpec spec)

targetComponent ::
  Maybe (Path Abs Dir) ->
  Maybe PackageName ->
  PackagesConfig ->
  TargetSpec ->
  M TargetOrDefault
targetComponent cliRoot mainPkg config spec = do
  root <- rootDir cliRoot
  targetComponentIn root mainPkg config spec

targetComponentOrError ::
  Maybe (Path Abs Dir) ->
  Maybe PackageName ->
  PackagesConfig ->
  TargetSpec ->
  M Target
targetComponentOrError cliRoot mainPkg config spec =
  targetComponent cliRoot mainPkg config spec >>= \case
    ExplicitTarget t -> pure t
    DefaultTarget t -> pure t
    NoDefaultTarget err -> clientError err
