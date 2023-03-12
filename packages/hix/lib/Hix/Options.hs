module Hix.Options where

import Hix.Optparse (absFileOption)
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
  switch,
  )
import Path (Abs, File, Path)
import Prelude hiding (Mod)

data PreprocOptions =
  PreprocOptions {
    source :: Path Abs File,
    inFile :: Path Abs File,
    outFile :: Path Abs File
  }
  deriving stock (Eq, Show, Generic)

data Command =
  Preproc PreprocOptions
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

preprocFileParser ::
  String ->
  String ->
  Parser (Path Abs File)
preprocFileParser longName helpText =
  option absFileOption (long longName <> completer (bashCompleter "file") <> help helpText)

preprocParser :: Parser PreprocOptions
preprocParser =
  PreprocOptions
  <$>
  preprocFileParser "source" "The original source file"
  <*>
  preprocFileParser "in" "The prepared input file"
  <*>
  preprocFileParser "out" "The path to the output file"

preprocCommand ::
  Mod CommandFields Command
preprocCommand =
  command "preproc" (Preproc <$> info (preprocParser) (progDesc "Preprocess a source file for use with ghcid"))

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
