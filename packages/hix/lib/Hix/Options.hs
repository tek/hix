module Hix.Options where

import Hix.Optparse (absFileOption)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  argument,
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
    source :: Path Abs File
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
  Parser (Path Abs File)
preprocFileParser =
  argument absFileOption (completer (bashCompleter "file") <> help fileHelp)
  where
    fileHelp =
      "The file for which to generate the preprocessor"

preprocParser :: Parser PreprocOptions
preprocParser =
  PreprocOptions
  <$>
  preprocFileParser

preprocCommand ::
  Mod CommandFields Command
preprocCommand =
  command "preproc" (Preproc <$> info (preprocParser) (progDesc "Generate a preprocessor for a file"))

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
