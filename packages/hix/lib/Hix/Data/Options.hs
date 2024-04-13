module Hix.Data.Options where

import Path (Abs, Dir, File, Path, SomeBase)

import Hix.Data.BootstrapProjectConfig (BootstrapProjectConfig)
import Hix.Data.ComponentConfig (ComponentName, ModuleName, SourceDir)
import Hix.Data.EnvName (EnvName)
import Hix.Data.GhciConfig (ChangeDir, EnvConfig, GhciConfig, RunnerName)
import Hix.Data.GlobalOptions (GlobalOptions)
import Hix.Data.NewProjectConfig (NewProjectConfig)
import Hix.Data.PackageName (PackageName)
import Hix.Data.PreprocConfig (PreprocConfig)
import Hix.Managed.Cabal.Data.Config (CabalConfig)
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto)
import Hix.Managed.Data.Query (RawQuery)
import Hix.Managed.Data.StateFileConfig (StateFileConfig)
import Hix.Managed.Handlers.Build (SpecialBuildHandlers)
import Hix.Optparse (JsonConfig)

data PreprocOptions =
  PreprocOptions {
    config :: Maybe (Either PreprocConfig JsonConfig),
    root :: Maybe (Path Abs Dir),
    source :: Path Abs File,
    inFile :: Path Abs File,
    outFile :: Path Abs File
  }
  deriving stock (Show, Generic)

data PackageSpec =
  PackageSpec {
    name :: PackageName,
    dir :: Maybe (SomeBase Dir)
  }
  deriving stock (Eq, Show, Generic)

data ComponentSpec =
  ComponentSpec {
    name :: ComponentName,
    dir :: Maybe SourceDir
  }
  deriving stock (Eq, Show, Generic)

data ComponentCoords =
  ComponentCoords {
    package :: Maybe PackageSpec,
    component :: Maybe ComponentSpec
  }
  deriving stock (Eq, Show, Generic)

data TargetSpec =
  TargetForFile (Path Abs File)
  |
  TargetForComponent ComponentCoords
  deriving stock (Eq, Show, Generic)

data TestOptions =
  TestOptions {
    mod :: ModuleName,
    test :: Maybe Text,
    runner :: Maybe RunnerName,
    cd :: ChangeDir
  }
  deriving stock (Eq, Show, Generic)

data EnvRunnerOptions =
  EnvRunnerOptions {
    config :: Either EnvConfig JsonConfig,
    root :: Maybe (Path Abs Dir),
    component :: Maybe TargetSpec
  }
  deriving stock (Show, Generic)

newtype ExtraGhciOptions =
  ExtraGhciOptions Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

newtype ExtraGhcidOptions =
  ExtraGhcidOptions Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

data GhciOptions =
  GhciOptions {
    config :: Either GhciConfig JsonConfig,
    root :: Maybe (Path Abs Dir),
    component :: TargetSpec,
    test :: TestOptions,
    extra :: Maybe ExtraGhciOptions
  }
  deriving stock (Show, Generic)

data GhcidOptions =
  GhcidOptions {
    ghci :: GhciOptions,
    extra :: Maybe ExtraGhcidOptions
  }
  deriving stock (Show, Generic)

data NewOptions =
  NewOptions {
    config :: NewProjectConfig
  }
  deriving stock (Eq, Show, Generic)

data BootstrapOptions =
  BootstrapOptions {
    config :: BootstrapProjectConfig
  }
  deriving stock (Eq, Show, Generic)

data EnvRunnerCommandOptions =
  EnvRunnerCommandOptions {
    options :: EnvRunnerOptions,
    test :: TestOptions,
    extraGhci :: Maybe ExtraGhciOptions,
    extraGhcid :: Maybe ExtraGhcidOptions
  }
  deriving stock (Show, Generic)

data ProjectOptions =
  ProjectOptions {
    build :: BuildConfig,
    envs :: [EnvName],
    query :: RawQuery,
    readUpperBounds :: Bool,
    mergeBounds :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Default ProjectOptions where
  def = ProjectOptions {
    build = def,
    envs = [],
    query = [],
    readUpperBounds = False,
    mergeBounds = False
  }

projectOptions :: [EnvName] -> ProjectOptions
projectOptions envs = def {envs}

data ManagedOptions =
  ManagedOptions {
    context :: Either ProjectContextProto JsonConfig,
    project :: ProjectOptions,
    stateFile :: StateFileConfig,
    cabal :: CabalConfig,
    handlers :: Maybe SpecialBuildHandlers
  }
  deriving stock (Show, Generic)

data BumpOptions =
  BumpOptions {
    common :: ManagedOptions
  }
  deriving stock (Show, Generic)

data LowerOptions =
  LowerOptions {
    common :: ManagedOptions,
    initOnly :: Bool,
    reset :: Bool,
    stabilize :: Bool
  }
  deriving stock (Show)

data LowerCommand =
  LowerInitCmd LowerOptions
  |
  LowerOptimizeCmd LowerOptions
  |
  LowerStabilizeCmd LowerOptions
  |
  LowerAutoCmd LowerOptions
  deriving stock (Show)

data Command =
  Preproc PreprocOptions
  |
  EnvRunner EnvRunnerCommandOptions
  |
  GhcidCmd GhcidOptions
  |
  GhciCmd GhciOptions
  |
  NewCmd NewOptions
  |
  BootstrapCmd BootstrapOptions
  |
  BumpCmd BumpOptions
  |
  LowerCmd LowerCommand
  deriving stock (Show)

data Options =
  Options {
    global :: GlobalOptions,
    cmd :: Command
  }
  deriving stock (Show)
