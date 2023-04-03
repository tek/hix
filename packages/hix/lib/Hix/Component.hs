module Hix.Component where

import Control.Monad.Trans.Except (ExceptT)
import Data.List.Extra (firstJust)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import Path (Abs, File, Path, isProperPrefixOf, stripProperPrefix)
import Path.IO (getCurrentDir)

import Hix.Data.Error (Error, note)
import qualified Hix.Data.GhciConfig as GhciConfig
import Hix.Data.GhciConfig (ComponentConfig, PackageConfig (PackageConfig), PackagesConfig, SourceDir (SourceDir))
import qualified Hix.Options as Options
import Hix.Options (ComponentSpec (ComponentForFile, ComponentForModule), ModuleSpec)

componentForModule :: PackagesConfig -> ModuleSpec -> ExceptT Error IO (PackageConfig, ComponentConfig)
componentForModule config spec = do
  pkg <- note "No such package" (config !? spec.package)
  comp <- note "No component for source dir" (find matchSourceDir (Map.elems pkg.components))
  pure (pkg, comp)
  where
    matchSourceDir comp = elem @[] spec.sourceDir (coerce comp.sourceDirs)

componentForFile :: PackagesConfig -> Path Abs File -> ExceptT Error IO (PackageConfig, ComponentConfig)
componentForFile config file = do
  cwd <- getCurrentDir
  fileRel <- stripProperPrefix cwd file
  (pkg, subpath) <- note "No package contains this file" (firstJust (matchPackage fileRel) (Map.elems config))
  comp <- note "No component source dir contains this file" (find (matchSourceDir subpath) (Map.elems pkg.components))
  pure (pkg, comp)
  where
    matchPackage fileRel pkg@PackageConfig {src} = do
      subpath <- stripProperPrefix src fileRel
      pure (pkg, subpath)
    matchSourceDir subpath comp = any @[] (\ (SourceDir dir) -> isProperPrefixOf dir subpath) (coerce comp.sourceDirs)

targetComponent :: PackagesConfig -> ComponentSpec -> ExceptT Error IO (PackageConfig, ComponentConfig)
targetComponent config = \case
  ComponentForModule spec ->
    componentForModule config spec
  ComponentForFile spec ->
    componentForFile config spec
