module Hix.Integration.Options where

import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  command,
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
  showHelpOnEmpty,
  showHelpOnError,
  subparserInline,
  )
import Path (Abs, Dir, Path)
import Path.IO (getCurrentDir)
import Prelude hiding (Mod, mod)

import Hix.Integration.Data.Options (Command (..), HackageServeOptions (..), Options (..))
import Hix.Options (globalParser)
import Hix.Optparse (absFileOption)

hackageParser :: Parser HackageServeOptions
hackageParser = do
  portFile <- optional (option absFileOption (long "port-file" <> help "Write the port number to this file"))
  pure HackageServeOptions {..}

commands :: Mod CommandFields Command
commands =
  command "hackage" (HackageServe <$> info hackageParser (progDesc "Run a hackage server"))

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
    parserPrefs = prefs (showHelpOnEmpty <> showHelpOnError <> subparserInline)
    desc = fullDesc <> header "Test helpers for Hix"
