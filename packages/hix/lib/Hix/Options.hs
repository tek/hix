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
import Hix.Data.GhciConfig (EnvConfig, GhciConfig, RunnerName)
import qualified Hix.Data.NewProjectConfig
import Hix.Data.NewProjectConfig (NewProjectConfig (NewProjectConfig))
import Hix.Data.PreprocConfig (PreprocConfig)
import Hix.Optparse (JsonConfig, absFileOption, jsonOption)

data PreprocOptions =
  PreprocOptions {
    config :: Maybe (Either PreprocConfig JsonConfig),
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
    package :: PackageSpec,
    component :: ComponentSpec
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
    runner :: Maybe RunnerName
  }
  deriving stock (Eq, Show, Generic)

data EnvRunnerOptions =
  EnvRunnerOptions {
    config :: Either EnvConfig JsonConfig,
    component :: Maybe TargetSpec
  }
  deriving stock (Show, Generic)

data GhciOptions =
  GhciOptions {
    config :: Either GhciConfig JsonConfig,
    component :: TargetSpec,
    test :: TestOptions
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
    test :: TestOptions
  }
  deriving stock (Show, Generic)

data Command =
  Preproc PreprocOptions
  |
  EnvRunner EnvRunnerCommandOptions
  |
  GhcidCmd GhciOptions
  |
  GhciCmd GhciOptions
  |
  NewCmd NewOptions
  |
  BootstrapCmd BootstrapOptions
  deriving stock (Show)

data GlobalOptions =
  GlobalOptions {
    verbose :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

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

jsonConfigParser ::
  Parser JsonConfig
jsonConfigParser =
  option jsonOption (long "config" <> short 'c' <> help "The Hix-generated config, file or text")

preprocParser :: Parser PreprocOptions
preprocParser =
  PreprocOptions
  <$>
  (fmap Right <$> optional jsonConfigParser)
  <*>
  fileParser "source" "The original source file"
  <*>
  fileParser "in" "The prepared input file"
  <*>
  fileParser "out" "The path to the output file"

packageSpecParser :: Parser PackageSpec
packageSpecParser = do
  name <- strOption (long "package" <> short 'p' <> help "The name or directory of the test package")
  pure PackageSpec {name = PackageName name, dir = parseSomeDir (toString name)}

componentSpecParser :: Parser ComponentSpec
componentSpecParser = do
  name <- strOption (long "component" <> short 'c' <> help h <> value "test")
  pure ComponentSpec {name = ComponentName name, dir = SourceDir <$> parseRelDir (toString name)}
  where
    h = "The name or relative directory of the test component"

componentForModuleParser :: Parser ComponentCoords
componentForModuleParser =
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
  TargetForComponent <$> componentForModuleParser
  <|>
  componentForFileParser

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

moduleParser :: Parser ModuleName
moduleParser =
  strOption (long "module" <> short 'm' <> help "The module containing the test function" <> value "Main")

testOptionsParser :: Parser TestOptions
testOptionsParser = do
  test <- testParser
  runner <- runnerParser
  mod <- moduleParser
  pure TestOptions {..}

envParser :: Parser EnvRunnerCommandOptions
envParser = do
  options <- do
    config <- Right <$> jsonConfigParser
    component <- optional targetSpecParser
    pure EnvRunnerOptions {..}
  test <- testOptionsParser
  pure EnvRunnerCommandOptions {..}

ghciParser :: Parser GhciOptions
ghciParser = do
  config <- Right <$> jsonConfigParser
  component <- targetSpecParser
  test <- testOptionsParser
  pure GhciOptions {..}

newParser :: Parser NewOptions
newParser = do
  name <- strOption (long "name" <> short 'n' <> help "The name of the new project and its main package")
  packages <- switch (long "packages" <> short 'p' <> help "Store packages in the 'packages/' subdirectory")
  hixUrl <- strOption (long "hix-url" <> help "The URL to the Hix repository" <> value def)
  author <- strOption (long "author" <> short 'a' <> help "Your name")
  pure NewOptions {config = NewProjectConfig {..}}

bootstrapParser :: Parser BootstrapOptions
bootstrapParser = do
  hixUrl <- strOption (long "hix-url" <> help "The URL to the Hix repository" <> value def)
  pure BootstrapOptions {config = BootstrapProjectConfig {..}}

commands ::
  Mod CommandFields Command
commands =
  command "preproc" (Preproc <$> info preprocParser (progDesc "Preprocess a source file for use with ghcid"))
  <>
  command "env" (EnvRunner <$> info envParser (progDesc "Print the env runner for a component or a named env"))
  <>
  command "ghci-cmd" (GhciCmd <$> info ghciParser (progDesc "Print a ghci cmdline to load a module in a Hix env"))
  <>
  command "ghcid-cmd" (GhcidCmd <$> info ghciParser (progDesc "Print a ghcid cmdline to run a function in a Hix env"))
  <>
  command "new" (NewCmd <$> info newParser (progDesc "Create a new Hix project in the current directory"))
  <>
  command "bootstrap" (BootstrapCmd <$> info bootstrapParser (progDesc "Bootstrap an existing Cabal project in the current directory"))

globalParser :: Parser GlobalOptions
globalParser = do
  verbose <- optional (switch (long "verbose" <> short 'v' <> help "Verbose output"))
  pure GlobalOptions {..}

appParser ::
  Parser Options
appParser =
  Options <$> globalParser <*> hsubparser commands

parseCli ::
  IO Options
parseCli = do
  customExecParser parserPrefs (info (appParser <**> helper) desc)
  where
    parserPrefs =
      prefs (showHelpOnEmpty <> showHelpOnError)
    desc =
      fullDesc <> header "Tools for maintaining Hix projects"
