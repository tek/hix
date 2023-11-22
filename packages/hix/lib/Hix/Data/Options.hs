module Hix.Data.Options where

import Path (Abs, Dir, File, Path, SomeBase)
import Prelude hiding (Mod, mod)

import Hix.Data.BootstrapProjectConfig (BootstrapProjectConfig)
import Hix.Data.ComponentConfig (ComponentName, ModuleName, SourceDir)
import Hix.Data.GhciConfig (ChangeDir, EnvConfig, GhciConfig, RunnerName)
import Hix.Data.LowerConfig (LowerInitConfig, LowerOptimizeConfig)
import Hix.Data.ManagedEnv (ManagedEnv)
import Hix.Data.NewProjectConfig (NewProjectConfig)
import Hix.Data.OutputFormat (OutputFormat)
import Hix.Data.Package (PackageName)
import Hix.Data.PreprocConfig (PreprocConfig)
import Hix.Managed.Data.ManagedConfig (ManagedConfig)
import Hix.Managed.Handlers.Bump (SpecialBumpHandlers)
import Hix.Managed.Handlers.LowerInit (SpecialLowerInitHandlers)
import Hix.Managed.Handlers.LowerOptimize (SpecialLowerOptimizeHandlers)
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

data BumpOptions =
  BumpOptions {
    env :: Either ManagedEnv JsonConfig,
    config :: ManagedConfig,
    handlers :: Maybe SpecialBumpHandlers
  }
  deriving stock (Show, Generic)

data LowerInitOptions =
  LowerInitOptions {
    env :: Either ManagedEnv JsonConfig,
    config :: ManagedConfig,
    lowerInit :: LowerInitConfig,
    handlers :: Maybe SpecialLowerInitHandlers
  }
  deriving stock (Show)

data LowerOptimizeOptions =
  LowerOptimizeOptions {
    env :: Either ManagedEnv JsonConfig,
    config :: ManagedConfig,
    lowerOptimize :: LowerOptimizeConfig,
    handlers :: Maybe SpecialLowerOptimizeHandlers
  }
  deriving stock (Show)

data LowerCommand =
  LowerInitCmd LowerInitOptions
  |
  LowerOptimizeCmd LowerOptimizeOptions
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

data GlobalOptions =
  GlobalOptions {
    -- TODO why is this Maybe? we're always using @fromMaybe False@ for it
    verbose :: Maybe Bool,
    debug :: Bool,
    quiet :: Bool,
    cwd :: Path Abs Dir,
    output :: OutputFormat
  }
  deriving stock (Eq, Show, Generic)

data Options =
  Options {
    global :: GlobalOptions,
    cmd :: Command
  }
  deriving stock (Show)
