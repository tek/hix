module Hix.Options where

import Exon (exon)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  ReadM,
  auto,
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
  metavar,
  option,
  prefs,
  progDesc,
  short,
  showDefault,
  showHelpOnEmpty,
  showHelpOnError,
  strArgument,
  strOption,
  subparserInline,
  switch,
  value,
  )
import Path (Abs, Dir, File, Path, SomeBase, parseRelDir, parseSomeDir)
import Path.IO (getCurrentDir)
import Prelude hiding (Mod, mod)

import qualified Hix.Data.BootstrapProjectConfig
import Hix.Data.BootstrapProjectConfig (BootstrapProjectConfig (BootstrapProjectConfig))
import Hix.Data.ComponentConfig (ComponentName (ComponentName), ModuleName, SourceDir (SourceDir))
import Hix.Data.EnvName (EnvName)
import Hix.Data.GhciConfig (ChangeDir (ChangeDir), RunnerName)
import Hix.Data.GlobalOptions (GlobalOptions (..))
import qualified Hix.Data.NewProjectConfig
import Hix.Data.NewProjectConfig (NewProjectConfig (NewProjectConfig))
import qualified Hix.Data.Options
import Hix.Data.Options (
  BootstrapOptions (..),
  BumpOptions (..),
  Command (..),
  ComponentCoords (ComponentCoords),
  ComponentSpec (..),
  EnvRunnerCommandOptions (..),
  EnvRunnerOptions (EnvRunnerOptions, component, config, root),
  ExtraGhciOptions,
  ExtraGhcidOptions,
  GhciOptions (..),
  GhcidOptions (..),
  LowerCommand (..),
  LowerOptions (LowerOptions),
  ManagedOptions (ManagedOptions),
  NewOptions (..),
  Options (Options),
  PackageSpec (..),
  PreprocOptions (PreprocOptions),
  ProjectOptions (ProjectOptions),
  TargetSpec (..),
  TestOptions (..),
  )
import Hix.Data.OutputFormat (OutputFormat (OutputNone))
import Hix.Data.OutputTarget (OutputTarget (OutputDefault))
import Hix.Data.PackageName (PackageName (PackageName))
import qualified Hix.Managed.Cabal.Data.Config
import Hix.Managed.Cabal.Data.Config (CabalConfig (CabalConfig))
import qualified Hix.Managed.Data.BuildConfig
import Hix.Managed.Data.BuildConfig (BuildConfig (BuildConfig))
import Hix.Managed.Data.Query (RawQuery (RawQuery))
import qualified Hix.Managed.Data.StateFileConfig
import Hix.Managed.Data.StateFileConfig (StateFileConfig (StateFileConfig))
import Hix.Managed.Handlers.Build (BuildTimeout (BuildTimeout))
import Hix.Optparse (
  JsonConfig,
  absDirOption,
  absFileOption,
  absFileOrCwdOption,
  buildHandlersOption,
  indexStateOption,
  jsonOption,
  outputFormatOption,
  outputTargetOption,
  relFileOption,
  someFileOption,
  )

fileParser ::
  ReadM a ->
  String ->
  String ->
  Parser a
fileParser readOption longName helpText =
  option readOption (long longName <> completer (bashCompleter "file") <> help helpText)

absFileParser :: String -> String -> Parser (Path Abs File)
absFileParser = fileParser absFileOption

absFileOrCwdParser :: Path Abs Dir -> String -> String -> Parser (Path Abs File)
absFileOrCwdParser cwd = fileParser (absFileOrCwdOption cwd)

someFileParser :: String -> String -> Parser (SomeBase File)
someFileParser = fileParser someFileOption

rootParser :: Parser (Maybe (Path Abs Dir))
rootParser =
  optional (option absDirOption (long "root" <> help "The root directory of the project"))

jsonConfigParser ::
  Parser JsonConfig
jsonConfigParser =
  option jsonOption (long "config" <> help "The Hix-generated config, file or text")

preprocParser ::
  Path Abs Dir ->
  Parser PreprocOptions
preprocParser cwd =
  PreprocOptions
  <$>
  (fmap Right <$> optional jsonConfigParser)
  <*>
  rootParser
  <*>
  absFileOrCwdParser cwd "source" "The original source file"
  <*>
  absFileOrCwdParser cwd "in" "The prepared input file"
  <*>
  absFileOrCwdParser cwd "out" "The path to the output file"

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

componentForFileParser ::
  Path Abs Dir ->
  Parser TargetSpec
componentForFileParser cwd =
  TargetForFile
  <$>
  option (absFileOrCwdOption cwd) (long "file" <> short 'f' <> help "The absolute file path of the test module")

targetSpecParser ::
  Path Abs Dir ->
  Parser TargetSpec
targetSpecParser cwd =
  componentForFileParser cwd
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

envParser ::
  Path Abs Dir ->
  Parser EnvRunnerCommandOptions
envParser cwd = do
  options <- do
    config <- Right <$> jsonConfigParser
    root <- rootParser
    component <- optional (targetSpecParser cwd)
    pure EnvRunnerOptions {..}
  test <- testOptionsParser
  extraGhci <- extraGhciParser
  extraGhcid <- extraGhcidParser
  pure EnvRunnerCommandOptions {..}

ghciParser ::
  Path Abs Dir ->
  Parser GhciOptions
ghciParser cwd = do
  config <- Right <$> jsonConfigParser
  root <- rootParser
  component <- targetSpecParser cwd
  test <- testOptionsParser
  extra <- extraGhciParser
  pure GhciOptions {..}

ghcidParser ::
  Path Abs Dir ->
  Parser GhcidOptions
ghcidParser cwd = do
  ghci <- ghciParser cwd
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

stateFileConfigParser :: Parser StateFileConfig
stateFileConfigParser = do
  file <- option relFileOption (long "file" <> short 'f' <> help "The relative path to the managed deps file")
  -- TODO this isn't nice. maybe there should be two options, since the user might want to build bumps without modifying
  -- the state.
  projectRoot <- optional (option absDirOption (long "root" <> help "The root directory of the project"))
  pure StateFileConfig {..}

cabalConfigParser :: Parser CabalConfig
cabalConfigParser = do
  indexState <- optional (option indexStateOption (long "index-state" <> help indexStateHelp))
  pure CabalConfig {..}
  where
    indexStateHelp = "The index state for Hackage, Unix stamp or date string"

buildConfigParser :: Parser BuildConfig
buildConfigParser = do
  maxIterations <- option auto (long "max-iterations" <> help maxIterationsHelp <> value 3 <> showDefault)
  maxFailedPre <- option auto (long "max-failed-majors-pre" <> help maxFailedPreHelp <> value 99 <> showDefault)
  maxFailedPost <- option auto (long "max-failed-majors-post" <> help maxFailedPostHelp <> value 0 <> showDefault)
  lookup <- switch (long "lookup" <> short 'n' <> help "Only print latest versions (bump)")
  validate <- switch (long "validate" <> help "Validate new versions, but don't update the project state")
  buildOutput <- switch (long "build-output" <> help "Show output from Nix builds")
  timeout <- fmap BuildTimeout <$> optional (option auto (long "build-timeout" <> help buildTimeoutHelp))
  disableNixMonitor <- switch (long "disable-nix-monitor" <> help "Don't parse Nix build output")
  pure BuildConfig {..}
  where
    maxIterationsHelp = "Number of restarts when some dependencies fail"
    maxFailedPreHelp = maxFailedHelp "prior to the first"
    maxFailedPostHelp = maxFailedHelp "after the last"
    maxFailedHelp variant = [exon|Number of majors that may fail before aborting, #{variant} success|]
    buildTimeoutHelp = "Kill Nix builds after this duration in seconds"
    toposortMutations = True

projectOptionsParser :: Parser ProjectOptions
projectOptionsParser = do
  build <- buildConfigParser
  query <- RawQuery <$> many (strArgument (help "Positional arguments select individual deps for processing"))
  envs <- some (strOption (long "env" <> short 'e' <> help "Environments whose packages should be updated"))
  readUpperBounds <- switch (long "read-upper-bounds" <> help "Use the upper bounds from the flake for the first run")
  mergeBounds <- switch (long "merge-bounds" <> help "Always add the flake bounds to the managed bounds")
  pure ProjectOptions {..}

managedOptionsParser :: Parser ManagedOptions
managedOptionsParser = do
  context <- Right <$> jsonConfigParser
  project <- projectOptionsParser
  stateFile <- stateFileConfigParser
  handlers <- optional (option buildHandlersOption (long "handlers" <> help "Internal: Handlers for tests"))
  cabal <- cabalConfigParser
  pure ManagedOptions {..}

bumpParser :: Parser BumpOptions
bumpParser = do
  common <- managedOptionsParser
  pure BumpOptions {..}

lowerParser :: Parser LowerOptions
lowerParser = do
  initOnly <- switch (long "init" <> help "Only initialize missing lower bounds")
  reset <- switch (long "reset" <> help "Reinitialize bounds of all deps rather than just new ones")
  stabilize <- switch (long "stabilize" <> help "Attempt to find working bounds if the current ones are broken")
  common <- managedOptionsParser
  pure LowerOptions {..}

lowerCommands :: Mod CommandFields LowerCommand
lowerCommands =
  command "init" (LowerInitCmd <$> info lowerParser (progDesc "Initialize the lower bounds"))
  <>
  command "optimize" (LowerOptimizeCmd <$> info lowerParser (progDesc "Optimize the lower bounds"))
  <>
  command "stabilize" (LowerStabilizeCmd <$> info lowerParser (progDesc "Stabilize the lower bounds"))
  <>
  command "auto" (LowerAutoCmd <$> info lowerParser (progDesc "Process the lower bounds"))

lowerCommand :: Parser LowerCommand
lowerCommand =
  hsubparser lowerCommands <|> (LowerAutoCmd <$> lowerParser)

managedCommitMsgParser :: Parser (Path Abs File)
managedCommitMsgParser =
  option absFileOption (long "file" <> help "The JSON file written by a managed deps app")

managedGithubPrParser :: Parser (Path Abs File)
managedGithubPrParser =
  option absFileOption (long "file" <> help "The JSON file written by a managed deps app")

commands ::
  Path Abs Dir ->
  Mod CommandFields Command
commands cwd =
  command "preproc" (Preproc <$> info (preprocParser cwd) (progDesc "Preprocess a source file for use with ghcid"))
  <>
  command "env" (EnvRunner <$> info (envParser cwd) (progDesc "Print the env runner for a component or a named env"))
  <>
  command "ghci-cmd" (GhciCmd <$> info (ghciParser cwd) (progDesc "Print a ghci cmdline to load a module in a Hix env"))
  <>
  command "ghcid-cmd" (GhcidCmd <$> info (ghcidParser cwd) (progDesc "Print a ghcid cmdline to run a function in a Hix env"))
  <>
  command "new" (NewCmd <$> info newParser (progDesc "Create a new Hix project in the current directory"))
  <>
  command "bootstrap" (BootstrapCmd <$> info bootstrapParser (progDesc bootstrapDesc))
  <>
  command "bump" (BumpCmd <$> info bumpParser (progDesc "Bump the deps of a package"))
  <>
  command "lower" (LowerCmd <$> info lowerCommand (progDesc "Modify the lower bounds of a package"))
  where
    bootstrapDesc = "Bootstrap an existing Cabal project in the current directory"

globalParser ::
  Path Abs Dir ->
  Parser GlobalOptions
globalParser realCwd = do
  verbose <- switch (long "verbose" <> short 'v' <> help "Verbose output")
  debug <- switch (long "debug" <> help "Debug output")
  quiet <- switch (long "quiet" <> help "Suppress info output")
  cwd <- option absDirOption (long "cwd" <> help "Force a different working directory" <> value realCwd)
  output <- option outputFormatOption (long "output" <> help formatHelp <> value OutputNone <> metavar "format")
  target <- option outputTargetOption (long "target" <> help targetHelp <> value OutputDefault <> metavar "target")
  pure GlobalOptions {..}
  where
    formatHelp = "Result output format, if commands support it"
    targetHelp = "Force output to a file or stdout"

appParser ::
  Path Abs Dir ->
  Parser Options
appParser cwd =
  Options <$> globalParser cwd <*> hsubparser (commands cwd)

parseCli ::
  IO Options
parseCli = do
  realCwd <- getCurrentDir
  customExecParser parserPrefs (info (appParser realCwd <**> helper) desc)
  where
    parserPrefs =
      prefs (showHelpOnEmpty <> showHelpOnError <> subparserInline)
    desc =
      fullDesc <> header "Tools for maintaining Hix projects"
