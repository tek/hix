module Hix.Options where

import Data.Aeson (FromJSON)
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
import Path (Abs, File, Path, reldir)
import Prelude hiding (Mod)

import Hix.Data.GhciConfig (EnvConfig, EnvName, GhciConfig, ModuleName, PackageName, RunnerName, SourceDir (SourceDir))
import Hix.Optparse (absFileOption, jsonOption, relDirOption)

data PreprocOptions =
  PreprocOptions {
    source :: Path Abs File,
    inFile :: Path Abs File,
    outFile :: Path Abs File
  }
  deriving stock (Eq, Show, Generic)

data ModuleSpec =
  ModuleSpec {
    package :: PackageName,
    sourceDir :: SourceDir,
    mod :: ModuleName
  }
  deriving stock (Eq, Show, Generic)

data ComponentSpec =
  ComponentForFile (Path Abs File)
  |
  ComponentForModule ModuleSpec
  deriving stock (Eq, Show, Generic)

data TestOptions =
  TestOptions {
    test :: Maybe Text,
    runner :: Maybe RunnerName
  }
  deriving stock (Eq, Show, Generic)

data EnvRunnerOptions =
  EnvRunnerOptions {
    config :: EnvConfig,
    component :: Maybe ComponentSpec
  }
  deriving stock (Eq, Show, Generic)

data GhciOptions =
  GhciOptions {
    config :: GhciConfig,
    component :: ComponentSpec,
    test :: TestOptions
  }
  deriving stock (Eq, Show, Generic)

data EnvRunnerCommandOptions =
  EnvRunnerCommandOptions {
    options :: EnvRunnerOptions,
    test :: TestOptions
  }
  deriving stock (Eq, Show, Generic)

data Command =
  Preproc PreprocOptions
  |
  EnvRunner EnvRunnerCommandOptions
  |
  GhcidCmd GhciOptions
  |
  GhciCmd GhciOptions
  deriving stock (Eq, Show)

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
  deriving stock (Eq, Show)

fileParser ::
  String ->
  String ->
  Parser (Path Abs File)
fileParser longName helpText =
  option absFileOption (long longName <> completer (bashCompleter "file") <> help helpText)

preprocParser :: Parser PreprocOptions
preprocParser =
  PreprocOptions
  <$>
  fileParser "source" "The original source file"
  <*>
  fileParser "in" "The prepared input file"
  <*>
  fileParser "out" "The path to the output file"

-- TODO also allow package by reldir and component by name
componentForModuleParser :: Parser ModuleSpec
componentForModuleParser =
  ModuleSpec
  <$>
  strOption (long "package" <> short 'p' <> help "The name of the test package")
  <*>
  (SourceDir <$> (option relDirOption (long "source-dir" <> short 'd' <> help sourceDirHelp <> value [reldir|test|])))
  <*>
  strOption (long "module" <> short 'm' <> help "The module containing the test function" <> value "Main")
  where
    sourceDirHelp = "The relative path to the component in the package"

componentForFileParser :: Parser ComponentSpec
componentForFileParser =
  ComponentForFile
  <$>
  option absFileOption (long "file" <> short 'f' <> help "The absolute file path of the test module")

componentSpecParser :: Parser ComponentSpec
componentSpecParser =
  ComponentForModule <$> componentForModuleParser
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

jsonConfigParser ::
  FromJSON a =>
  Parser a
jsonConfigParser =
  option jsonOption (long "config" <> short 'c' <> help "The Hix-generated config")

testOptionsParser :: Parser TestOptions
testOptionsParser = do
  test <- testParser
  runner <- runnerParser
  pure TestOptions {..}

envParser :: Parser EnvRunnerCommandOptions
envParser = do
  options <- do
    config <- jsonConfigParser
    component <- optional componentSpecParser
    pure EnvRunnerOptions {..}
  test <- testOptionsParser
  pure EnvRunnerCommandOptions {..}

ghciParser :: Parser GhciOptions
ghciParser = do
  config <- jsonConfigParser
  component <- componentSpecParser
  test <- testOptionsParser
  pure GhciOptions {..}

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
