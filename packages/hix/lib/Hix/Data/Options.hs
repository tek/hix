module Hix.Data.Options where

import Path (Dir, File, SomeBase)
import Text.Show (show)

import Hix.Data.BootstrapProjectConfig (BootstrapProjectConfig)
import Hix.Data.ComponentConfig (ComponentName, ModuleName, SourceDir)
import Hix.Data.EnvName (EnvName)
import Hix.Data.GhciConfig (ChangeDir, CommandContext, GhciArgs, GhciContext, GhcidArgs, RunnerName)
import Hix.Data.GlobalOptions (GlobalOptions)
import Hix.Data.Json (JsonContext)
import Hix.Data.NewProjectConfig (InitProjectConfig, NewProjectConfig)
import Hix.Data.PackageName (PackageName)
import Hix.Data.PathSpec (PathSpec)
import Hix.Data.PreprocConfig (PreprocConfig)
import Hix.Managed.Cabal.Data.ContextHackageRepo (ContextHackageRepo)
import Hix.Managed.Cabal.Data.HackageRepo (HackageName)
import Hix.Managed.Data.BuildConfig (BuildConfig, SpecialBuildHandlers)
import Hix.Managed.Data.MaintConfig (MaintConfig)
import Hix.Managed.Data.MaintContext (MaintContextProto)
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto)
import Hix.Managed.Data.Query (RawQuery)
import Hix.Managed.Data.RevisionConfig (RevisionConfig)
import Hix.Managed.Data.SpecialMaintHandlers (SpecialMaintHandlers)
import Hix.Managed.Data.StateFileConfig (StateFileConfig)

data InfoCommand =
  InfoVersion
  deriving stock (Eq, Show)

data PreprocOptions =
  PreprocOptions {
    config :: Maybe (Either PreprocConfig JsonContext),
    root :: Maybe (PathSpec Dir),
    source :: PathSpec File,
    inFile :: PathSpec File,
    outFile :: PathSpec File
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
  TargetForFile (PathSpec File)
  |
  TargetForComponent ComponentCoords
  deriving stock (Eq, Show, Generic)

defaultTargetSpec :: TargetSpec
defaultTargetSpec =
  TargetForComponent ComponentCoords {package = Nothing, component = Nothing}

data TestOptions =
  TestOptions {
    mod :: ModuleName,
    test :: Maybe Text,
    runner :: Maybe RunnerName,
    cd :: ChangeDir
  }
  deriving stock (Eq, Show, Generic)

data CommandOptions =
  CommandOptions {
    root :: Maybe (PathSpec Dir),
    component :: Maybe TargetSpec,
    env :: Maybe EnvName
  }
  deriving stock (Show, Generic)

data GhciOptions =
  GhciOptions {
    context :: Either GhciContext JsonContext,
    command :: CommandOptions,
    test :: TestOptions,
    extra :: GhciArgs,
    args :: [Text]
  }
  deriving stock (Show, Generic)

data GhcidOptions =
  GhcidOptions {
    ghci :: GhciOptions,
    extra :: GhcidArgs
  }
  deriving stock (Show, Generic)

data RunCommandOptions =
  RunCommandOptions {
    context :: Either CommandContext JsonContext,
    command :: CommandOptions,
    exe :: Text,
    args :: [Text]
  }
  deriving stock (Show)

data InitOptions =
  InitOptions {
    config :: InitProjectConfig
  }
  deriving stock (Eq, Show, Generic)

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
    options :: RunCommandOptions,
    test :: TestOptions,
    extraGhci :: GhciArgs,
    extraGhcid :: GhcidArgs
  }
  deriving stock (Show, Generic)

data CabalOptions =
  CabalOptions {
    hackage :: [(HackageName, ContextHackageRepo -> ContextHackageRepo)]
  }

instance Show CabalOptions where
  show CabalOptions {} = "CabalOptions"

instance Default CabalOptions where
  def = CabalOptions {hackage = []}

data ProjectOptions =
  ProjectOptions {
    build :: BuildConfig,
    cabal :: CabalOptions,
    envs :: [EnvName],
    query :: RawQuery,
    readUpperBounds :: Bool,
    mergeBounds :: Bool,
    localDeps :: Bool
  }
  deriving stock (Show, Generic)

instance Default ProjectOptions where
  def = ProjectOptions {
    build = def,
    cabal = def,
    envs = [],
    query = [],
    readUpperBounds = False,
    mergeBounds = False,
    localDeps = False
  }

projectOptions :: [EnvName] -> ProjectOptions
projectOptions envs = def {envs}

data ManagedOptions =
  ManagedOptions {
    context :: Either ProjectContextProto (Maybe JsonContext),
    project :: ProjectOptions,
    stateFile :: StateFileConfig,
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
  LowerInit LowerOptions
  |
  LowerOptimize LowerOptions
  |
  LowerStabilize LowerOptions
  |
  LowerAuto LowerOptions
  deriving stock (Show)

data ReleaseMaintOptions =
  ReleaseMaintOptions {
    context :: Either MaintContextProto (Maybe JsonContext),
    managed :: ManagedOptions,
    handlers :: Maybe SpecialMaintHandlers,
    config :: MaintConfig
  }
  deriving stock (Show)

data RevisionOptions =
  RevisionOptions {
    context :: Either MaintContextProto (Maybe JsonContext),
    config :: RevisionConfig,
    cabal :: CabalOptions
  }
  deriving stock (Show)

data HackageCommand =
  ReleaseMaint ReleaseMaintOptions
  |
  Revision RevisionOptions
  deriving stock (Show)

data Command =
  Info InfoCommand
  |
  Preproc PreprocOptions
  |
  EnvRunner EnvRunnerCommandOptions
  |
  GhciCmd GhciOptions
  |
  GhcidCmd GhcidOptions
  |
  RunGhci GhciOptions
  |
  RunGhcid GhcidOptions
  |
  RunCommand RunCommandOptions
  |
  Init InitOptions
  |
  New NewOptions
  |
  Bootstrap BootstrapOptions
  |
  Bump BumpOptions
  |
  Lower LowerCommand
  |
  Hackage HackageCommand
  deriving stock (Show)

data Options =
  Options {
    global :: GlobalOptions,
    cmd :: Command
  }
  deriving stock (Show)
