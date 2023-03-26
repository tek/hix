module Hix.Options where

import Hix.Data.GhciConfig (GhciConfig, PackageName, RunnerName, SourceDir (SourceDir))
import Hix.Optparse (absFileOption, jsonOption, relDirOption)
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

data PreprocOptions =
  PreprocOptions {
    source :: Path Abs File,
    inFile :: Path Abs File,
    outFile :: Path Abs File
  }
  deriving stock (Eq, Show, Generic)

newtype GhcidFileSpec =
  GhcidFileSpec { unGhcidFileSpec :: Path Abs File }
  deriving stock (Eq, Show, Generic)

data GhcidModuleSpec =
  GhcidModuleSpec {
    package :: PackageName,
    sourceDir :: SourceDir,
    module_ :: Text
  }
  deriving stock (Eq, Show, Generic)

data GhciOptions =
  GhciOptions {
    config :: GhciConfig,
    spec :: Either GhcidFileSpec GhcidModuleSpec,
    test :: Maybe Text,
    runner :: RunnerName
  }
  deriving stock (Eq, Show, Generic)

data Command =
  Preproc PreprocOptions
  |
  GhciEnv GhciOptions
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
moduleSpecParser :: Parser GhcidModuleSpec
moduleSpecParser =
  GhcidModuleSpec
  <$>
  strOption (long "package" <> short 'p' <> help "The name of the test package")
  <*>
  (SourceDir <$> (option relDirOption (long "source-dir" <> short 'd' <> help sourceDirHelp <> value [reldir|test|])))
  <*>
  strOption (long "module" <> short 'm' <> help "The module containing the test function" <> value "Main")
  where
    sourceDirHelp = "The relative path to the component in the package"

fileSpecParser :: Parser GhcidFileSpec
fileSpecParser =
  GhcidFileSpec
  <$>
  option absFileOption (long "file" <> short 'f' <> help "The absolute file path of the test module")

specParser :: Parser (Either GhcidFileSpec GhcidModuleSpec)
specParser =
  (Right <$> moduleSpecParser)
  <|>
  (Left <$> fileSpecParser)

ghcidParser :: Parser GhciOptions
ghcidParser =
  GhciOptions
  <$>
  option jsonOption (long "config" <> short 'c' <> help "The Hix-generated config")
  <*>
  specParser
  <*>
  optional (strOption (long "test" <> short 't' <> help "The test function that will be executed"))
  <*>
  strOption (long "runner" <> help "The runner function that can execute the test function as IO" <> value "generic")

preprocCommand ::
  Mod CommandFields Command
preprocCommand =
  command "preproc" (Preproc <$> info preprocParser (progDesc "Preprocess a source file for use with ghcid"))
  <>
  command "ghci-env" (GhciEnv <$> info ghcidParser (progDesc "Find the env runner for a component"))
  <>
  command "ghci-cmd" (GhciCmd <$> info ghcidParser (progDesc "Print a ghci cmdline to load a module in a Hix env"))
  <>
  command "ghcid-cmd" (GhcidCmd <$> info ghcidParser (progDesc "Print a ghcid cmdline to run a function in a Hix env"))

globalParser :: Parser GlobalOptions
globalParser = do
  verbose <- optional (switch (long "verbose" <> short 'v' <> help "Verbose output"))
  pure GlobalOptions {..}

appParser ::
  Parser Options
appParser =
  Options <$> globalParser <*> hsubparser (mconcat [preprocCommand])

parseCli ::
  IO Options
parseCli = do
  customExecParser parserPrefs (info (appParser <**> helper) desc)
  where
    parserPrefs =
      prefs (showHelpOnEmpty <> showHelpOnError)
    desc =
      fullDesc <> header "Tools for maintaining Hix projects"
