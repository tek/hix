module Hix.Options where

import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  bashCompleter,
  command,
  completer,
  customExecParser,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  long,
  option,
  prefs,
  progDesc,
  short,
  showHelpOnEmpty,
  showHelpOnError,
  strOption,
  switch,
  value,
  )
import Path (Abs, Dir, File, Path, SomeBase, parseRelDir, parseSomeDir)
import Path.IO (getCurrentDir)
import Prelude hiding (Mod, mod)

import qualified Hix.Data.BootstrapProjectConfig
import Hix.Data.BootstrapProjectConfig (BootstrapProjectConfig (BootstrapProjectConfig))
import Hix.Data.ComponentConfig (
  ComponentName (ComponentName),
  EnvName,
  ModuleName,
  PackageName (PackageName),
  SourceDir (SourceDir),
  )
import Hix.Data.GhciConfig (ChangeDir (ChangeDir), EnvConfig, GhciConfig, RunnerName)
import qualified Hix.Data.NewProjectConfig
import Hix.Data.NewProjectConfig (NewProjectConfig (NewProjectConfig))
import Hix.Data.PreprocConfig (PreprocConfig)
import Hix.Optparse (JsonConfig, absDirOption, absFileOption, jsonOption)

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
  deriving stock (Show)

data GlobalOptions =
  GlobalOptions {
    verbose :: Maybe Bool,
    cwd :: Path Abs Dir
  }
  deriving stock (Eq, Show, Generic)

data Options =
  Options {
    global :: GlobalOptions,
    cmd :: Command
  }
  deriving stock (Show)

fileParser ::
  String ->
  String ->
  Parser (Path Abs File)
fileParser longName helpText =
  option absFileOption (long longName <> completer (bashCompleter "file") <> help helpText)

rootParser :: Parser (Maybe (Path Abs Dir))
rootParser =
  optional (option absDirOption (long "root" <> help "The root directory of the project"))

jsonConfigParser ::
  Parser JsonConfig
jsonConfigParser =
  option jsonOption (long "config" <> help "The Hix-generated config, file or text")

preprocParser :: Parser PreprocOptions
preprocParser =
  PreprocOptions
  <$>
  (fmap Right <$> optional jsonConfigParser)
  <*>
  rootParser
  <*>
  fileParser "source" "The original source file"
  <*>
  fileParser "in" "The prepared input file"
  <*>
  fileParser "out" "The path to the output file"

packageSpecParser :: Parser (Maybe PackageSpec)
packageSpecParser = do
  optional (strOption (long "package" <> short 'p' <> help "The name or directory of the test package")) <&> fmap \ name ->
    PackageSpec {name = PackageName name, dir = parseSomeDir (toString name)}

componentSpecParser :: Parser (Maybe ComponentSpec)
componentSpecParser = do
  optional (strOption (long "component" <> short 'c' <> help h)) <&> fmap \ name ->
    ComponentSpec {name = ComponentName name, dir = SourceDir <$> parseRelDir (toString name)}
  where
    h = "The name or relative directory of the test component"

componentCoordsParser :: Parser ComponentCoords
componentCoordsParser =
  ComponentCoords
  <$>
  packageSpecParser
  <*>
  componentSpecParser

componentForFileParser :: Parser TargetSpec
componentForFileParser =
  TargetForFile
  <$>
  option absFileOption (long "file" <> short 'f' <> help "The absolute file path of the test module")

targetSpecParser :: Parser TargetSpec
targetSpecParser =
  componentForFileParser
  <|>
  TargetForComponent <$> componentCoordsParser

envNameParser :: Parser EnvName
envNameParser =
  strOption (long "env" <> short 'e' <> help "The name of the environment")

testParser :: Parser (Maybe Text)
testParser =
  optional (strOption (long "test" <> short 't' <> help "The Haskell function that should be executed"))

runnerParser :: Parser (Maybe RunnerName)
runnerParser =
  optional (strOption (
    long "run"
    <>
    short 'r'
    <>
    help "The name of the command defined in the Hix option 'ghci.run'"
  ))

cdParser :: Parser ChangeDir
cdParser =
  ChangeDir . not <$> switch (long "no-cd" <> help "Don't change the working directory to the package root")

moduleParser :: Parser ModuleName
moduleParser =
  strOption (long "module" <> short 'm' <> help "The module containing the test function" <> value "Main")

testOptionsParser :: Parser TestOptions
testOptionsParser = do
  test <- testParser
  runner <- runnerParser
  mod <- moduleParser
  cd <- cdParser
  pure TestOptions {..}

extraGhciParser :: Parser (Maybe ExtraGhciOptions)
extraGhciParser =
  optional (strOption (long "ghci-options" <> help "Additional command line options to pass to ghci"))

extraGhcidParser :: Parser (Maybe ExtraGhcidOptions)
extraGhcidParser =
  optional (strOption (long "ghcid-options" <> help "Additional command line options to pass to ghcid"))

envParser :: Parser EnvRunnerCommandOptions
envParser = do
  options <- do
    config <- Right <$> jsonConfigParser
    root <- rootParser
    component <- optional targetSpecParser
    pure EnvRunnerOptions {..}
  test <- testOptionsParser
  extraGhci <- extraGhciParser
  extraGhcid <- extraGhcidParser
  pure EnvRunnerCommandOptions {..}

ghciParser :: Parser GhciOptions
ghciParser = do
  config <- Right <$> jsonConfigParser
  root <- rootParser
  component <- targetSpecParser
  test <- testOptionsParser
  extra <- extraGhciParser
  pure GhciOptions {..}

ghcidParser :: Parser GhcidOptions
ghcidParser = do
  ghci <- ghciParser
  extra <- extraGhcidParser
  pure GhcidOptions {..}

newParser :: Parser NewOptions
newParser = do
  name <- strOption (long "name" <> short 'n' <> help "The name of the new project and its main package")
  packages <- switch (long "packages" <> short 'p' <> help "Store packages in the 'packages/' subdirectory")
  hixUrl <- strOption (long "hix-url" <> help "The URL to the Hix repository" <> value def)
  author <- strOption (long "author" <> short 'a' <> help "Your name" <> value "Author")
  pure NewOptions {config = NewProjectConfig {..}}

bootstrapParser :: Parser BootstrapOptions
bootstrapParser = do
  hixUrl <- strOption (long "hix-url" <> help "The URL to the Hix repository" <> value def)
  pure BootstrapOptions {config = BootstrapProjectConfig {..}}

commands :: Mod CommandFields Command
commands =
  command "preproc" (Preproc <$> info preprocParser (progDesc "Preprocess a source file for use with ghcid"))
  <>
  command "env" (EnvRunner <$> info envParser (progDesc "Print the env runner for a component or a named env"))
  <>
  command "ghci-cmd" (GhciCmd <$> info ghciParser (progDesc "Print a ghci cmdline to load a module in a Hix env"))
  <>
  command "ghcid-cmd" (GhcidCmd <$> info ghcidParser (progDesc "Print a ghcid cmdline to run a function in a Hix env"))
  <>
  command "new" (NewCmd <$> info newParser (progDesc "Create a new Hix project in the current directory"))
  <>
  command "bootstrap" (BootstrapCmd <$> info bootstrapParser (progDesc "Bootstrap an existing Cabal project in the current directory"))

globalParser ::
  Path Abs Dir ->
  Parser GlobalOptions
globalParser realCwd = do
  verbose <- optional (switch (long "verbose" <> short 'v' <> help "Verbose output"))
  cwd <- option absDirOption (long "cwd" <> help "Force a different working directory" <> value realCwd)
  pure GlobalOptions {..}

appParser ::
  Path Abs Dir ->
  Parser Options
appParser cwd =
  Options <$> globalParser cwd <*> hsubparser commands

parseCli ::
  IO Options
parseCli = do
  realCwd <- getCurrentDir
  customExecParser parserPrefs (info (appParser realCwd <**> helper) desc)
  where
    parserPrefs =
      prefs (showHelpOnEmpty <> showHelpOnError)
    desc =
      fullDesc <> header "Tools for maintaining Hix projects"
