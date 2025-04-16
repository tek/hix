module Hix.Options where

import Exon (exon)
import Options.Applicative (
  ArgumentFields,
  CommandFields,
  Mod,
  Parser,
  argument,
  auto,
  command,
  customExecParser,
  forwardOptions,
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
import Path (Abs, Dir, Path, SomeBase (Abs), parseRelDir, parseSomeDir, relfile)
import Path.IO (getCurrentDir)
import Prelude hiding (Mod, mod)

import qualified Hix.Data.BootstrapProjectConfig
import Hix.Data.BootstrapProjectConfig (BootstrapProjectConfig (BootstrapProjectConfig))
import Hix.Data.ComponentConfig (ComponentName (ComponentName), ModuleName, SourceDir (SourceDir))
import Hix.Data.EnvName (EnvName)
import Hix.Data.GhciConfig (ChangeDir (ChangeDir), RunnerName)
import Hix.Data.GlobalOptions (GlobalOptions (..))
import Hix.Data.Json (JsonConfig)
import Hix.Data.LogLevel (LogLevel (..))
import qualified Hix.Data.NewProjectConfig
import Hix.Data.NewProjectConfig (
  CreateProjectConfig (CreateProjectConfig),
  InitProjectConfig (InitProjectConfig),
  NewProjectConfig (NewProjectConfig),
  ProjectName,
  )
import qualified Hix.Data.Options
import Hix.Data.Options (
  BootstrapOptions (..),
  BumpOptions (..),
  CabalOptions (..),
  Command (..),
  CommandOptions (..),
  ComponentCoords (ComponentCoords),
  ComponentSpec (..),
  EnvRunnerCommandOptions (..),
  EnvRunnerOptions (EnvRunnerOptions, component, config, root),
  ExtraGhciOptions,
  ExtraGhcidOptions,
  GhciOptions (..),
  GhcidOptions (..),
  HackageCommand (..),
  InitOptions (..),
  LowerCommand (..),
  LowerOptions (LowerOptions),
  ManagedOptions (ManagedOptions),
  NewOptions (..),
  Options (Options),
  PackageSpec (..),
  PreprocOptions (PreprocOptions),
  ProjectOptions (ProjectOptions),
  ReleaseMaintOptions (..),
  RevisionOptions (..),
  TargetSpec (..),
  TestOptions (..),
  )
import Hix.Data.OutputFormat (OutputFormat (OutputNone))
import Hix.Data.OutputTarget (OutputTarget (OutputDefault))
import Hix.Data.PackageName (PackageName (PackageName))
import Hix.Data.PathSpec (PathSpec (PathConcrete))
import qualified Hix.Managed.Data.BuildConfig
import Hix.Managed.Data.BuildConfig (BuildConfig (BuildConfig), BuildTimeout (..))
import Hix.Managed.Data.MaintConfig (MaintConfig (..))
import Hix.Managed.Data.Query (RawQuery (RawQuery))
import Hix.Managed.Data.RevisionConfig (RevisionConfig (..))
import qualified Hix.Managed.Data.StateFileConfig
import Hix.Managed.Data.StateFileConfig (StateFileConfig (StateFileConfig))
import Hix.Optparse (
  buildHandlersOption,
  hackageRepoFieldOption,
  jsonOption,
  maintHandlersOption,
  nonOption,
  outputFormatOption,
  outputTargetOption,
  relFileOption,
  )

nonOptionArgument :: IsString a => Mod ArgumentFields a -> Parser a
nonOptionArgument = argument nonOption

rootParser :: Parser (Maybe (PathSpec Dir))
rootParser =
  optional (strOption (long "root" <> help "The root directory of the project"))

jsonConfigParser :: Parser JsonConfig
jsonConfigParser =
  option jsonOption (long "config" <> help "The Hix-generated config, file or text")

preprocParser :: Parser PreprocOptions
preprocParser =
  PreprocOptions . fmap Right
  <$>
  optional jsonConfigParser
  <*>
  rootParser
  <*>
  strOption (long "source" <> help "The original source file")
  <*>
  strOption (long "in" <> help "The prepared input file")
  <*>
  strOption (long "out" <> help "The path to the output file")

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
  strOption (long "file" <> short 'f' <> help "The absolute file path of the test module")

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

envRunnerParser :: Parser EnvRunnerOptions
envRunnerParser = do
  config <- Right <$> jsonConfigParser
  root <- rootParser
  component <- optional targetSpecParser
  pure EnvRunnerOptions {..}

envLegacyParser :: Parser EnvRunnerCommandOptions
envLegacyParser = do
  options <- envRunnerParser
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
  args <- many (strArgument (metavar "ARG"))
  pure GhciOptions {..}

ghcidParser :: Parser GhcidOptions
ghcidParser = do
  ghci <- ghciParser
  extra <- extraGhcidParser
  pure GhcidOptions {..}

commandParser :: Parser CommandOptions
commandParser = do
  env <- envRunnerParser
  exe <- strOption (long "exe" <> help "The command executable")
  args <- many (strArgument (metavar "ARG"))
  pure CommandOptions {..}

noInitGitAndFlakeParser :: Parser Bool
noInitGitAndFlakeParser = switch (long "basic" <> short 'b' <> help "Skip git repo initialisation and nix flake lock")

initCommonParser :: Parser CreateProjectConfig
initCommonParser = do
  packages <- switch (long "packages" <> short 'p' <> help "Store packages in the 'packages/' subdirectory")
  hixUrl <- strOption (long "hix-url" <> help "The URL to the Hix repository" <> value def)
  author <- strOption (long "author" <> short 'a' <> help "Your name" <> value "Author")
  noInitGitAndFlake <- noInitGitAndFlakeParser
  devCli <- switch (long "dev-cli" <> help "Build the CLI tool from the flake input commit, for testing")
  pure CreateProjectConfig {..}

projectNameParser :: Parser ProjectName
projectNameParser = strOption (long "name" <> short 'n' <> help "The name of the new project and its main package")

initParser :: Parser InitOptions
initParser = do
  name <- projectNameParser
  config <- initCommonParser
  pure InitOptions {config = InitProjectConfig {..}}

newParser :: Parser NewOptions
newParser = do
  directory <- nonOptionArgument (metavar "DIR" <> help "Directory to create for the project, last component used as project name default")
  name <- optional projectNameParser
  printDirectory <- switch (long "print-dir" <> help "Print the created directory to stdout")
  config <- initCommonParser
  pure $ NewOptions {config = NewProjectConfig {..}}

bootstrapParser :: Parser BootstrapOptions
bootstrapParser = do
  hixUrl <- strOption (long "hix-url" <> help "The URL to the Hix repository" <> value def)
  noInitGitAndFlake <- noInitGitAndFlakeParser
  devCli <- switch (long "dev-cli" <> help "Build the CLI tool from the flake input commit, for testing")
  pure BootstrapOptions {config = BootstrapProjectConfig {..}}

stateFileConfigParser :: Parser StateFileConfig
stateFileConfigParser = do
  -- TODO this needs to be moved to ProjectContextProto
  file <- option relFileOption (long "file" <> short 'f' <> help fileHelp <> value [relfile|ops/managed.nix|])
  pure StateFileConfig {..}
  where
    fileHelp = "The relative path to the managed deps file"

cabalOptionsParser :: Parser CabalOptions
cabalOptionsParser = do
  hackage <- many (option hackageRepoFieldOption (long "hackage" <> help hackageHelp))
  pure CabalOptions {..}
  where
    hackageHelp = [exon|Override Hackage server options, formatted #{fmt}|]
    fmt = "'<name>:<key>:<value>'"

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
  cabal <- cabalOptionsParser
  query <- RawQuery <$> many (nonOptionArgument (help "Positional arguments select individual deps for processing"))
  envs <- many (strOption (long "env" <> short 'e' <> help "Environments whose packages should be updated"))
  readUpperBounds <- switch (long "read-upper-bounds" <> help "Use the upper bounds from the flake for the first run")
  mergeBounds <- switch (long "merge-bounds" <> help "Always add the flake bounds to the managed bounds")
  localDeps <- switch (long "local-deps" <> help "Manage bounds of local deps belonging to separate sets")
  pure ProjectOptions {..}

managedOptionsParser :: Parser ManagedOptions
managedOptionsParser = do
  context <- Right <$> optional jsonConfigParser
  project <- projectOptionsParser
  stateFile <- stateFileConfigParser
  handlers <- optional (option buildHandlersOption (long "handlers" <> help "Internal: Handlers for tests"))
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
  command "init" (LowerInit <$> info lowerParser (progDesc "Initialize the lower bounds"))
  <>
  command "optimize" (LowerOptimize <$> info lowerParser (progDesc "Optimize the lower bounds"))
  <>
  command "stabilize" (LowerStabilize <$> info lowerParser (progDesc "Stabilize the lower bounds"))
  <>
  command "auto" (LowerAuto <$> info lowerParser (progDesc "Process the lower bounds"))

lowerCommand :: Parser LowerCommand
lowerCommand =
  hsubparser lowerCommands <|> (LowerAuto <$> lowerParser)

maintConfigParser :: Parser MaintConfig
maintConfigParser = do
  targets <- nonEmpty <$> many (strOption (long "package" <> help "Packages that should be processed"))
  noFailures <- switch (long "no-failures" <> help "Only commit or publish a package if no dependency update fails")
  commit <- switch (long "commit" <> help "Commit dependency updates")
  push <- switch (long "push" <> help "Commit and push dependency updates")
  pr <- switch (long "pr" <> help "Don't publish revisions and create branches with timestamps for PRs")
  revision <- switch (long "revision" <> help "Publish a revision to Hackage")
  globalGit <- switch (long "global-git" <> help "Use the global git ID rather than hix@tryp.io")
  fetch <- switch (long "fetch" <> help "Fetch tags and branches, useful in CI")
  pure MaintConfig {commit = commit || push || pr, push = push || pr, ..}

-- TODO Since this includes ManagedOptions, some options may be ineffective or counterproductive.
-- Needs to be adapted.
maintParser :: Parser ReleaseMaintOptions
maintParser = do
  context <- Right <$> optional jsonConfigParser
  managed <- managedOptionsParser
  handlers <- optional (option maintHandlersOption (long "handlers" <> help "Internal: Handlers for tests"))
  config <- maintConfigParser
  pure ReleaseMaintOptions {..}

revisionConfigParser :: Parser RevisionConfig
revisionConfigParser = do
  packages <- fmap Left <$> many (strOption (long "package" <> help "Packages that should be processed"))
  branches <- fmap Right <$> many (strOption (long "branch" <> help "Release branches that should be processed"))
  globalGit <- switch (long "global-git" <> help "Use the global git ID rather than hix@tryp.io")
  fetch <- switch (long "fetch" <> help "Fetch tags and branches, useful in CI")
  pure RevisionConfig {targets = nonEmpty (packages ++ branches), ..}

revisionParser :: Parser RevisionOptions
revisionParser = do
  context <- Right <$> optional jsonConfigParser
  config <- revisionConfigParser
  cabal <- cabalOptionsParser
  pure RevisionOptions {..}

hackageCommands :: Mod CommandFields HackageCommand
hackageCommands =
  command "maint" (ReleaseMaint <$> info maintParser (progDesc maintDesc))
  <>
  command "revision" (Revision <$> info revisionParser (progDesc "Publish revisions of previous releases"))
  where
    maintDesc = "Maintain dependency bounds of previous releases"

hackageCommand :: Parser HackageCommand
hackageCommand = hsubparser hackageCommands

commands :: Mod CommandFields Command
commands =
  command "preproc" (Preproc <$> info preprocParser (progDesc "Preprocess a source file for use with ghcid"))
  <>
  command "env" (EnvRunner <$> info envLegacyParser (progDesc "Print the env runner for a component or a named env"))
  <>
  command "ghci-cmd" (GhciCmd <$> info ghciParser (progDesc "Print a ghci cmdline to load a module in a Hix env"))
  <>
  command "ghcid-cmd" (GhcidCmd <$> info ghcidParser (progDesc "Print a ghcid cmdline to run a function in a Hix env"))
  <>
  command "ghci" (RunGhci <$> info ghciParser (progDesc "Run ghci in a Hix env"))
  <>
  command "ghcid" (RunGhcid <$> info ghcidParser (progDesc "Run ghcid in a Hix env"))
  <>
  command "command" (RunCommand <$> info commandParser (progDesc "Run a command in a Hix env"))
  <>
  command "init" (Init <$> info initParser (progDesc "Create a new Hix project in the current directory"))
  <>
  command "new" (New <$> info newParser (progDesc "Create a new Hix project in the specified directory"))
  <>
  command "bootstrap" (Bootstrap <$> info bootstrapParser (progDesc bootstrapDesc))
  <>
  command "bump" (Bump <$> info bumpParser (progDesc "Bump the deps of a package"))
  <>
  command "lower" (Lower <$> info lowerCommand (progDesc "Modify the lower bounds of a package"))
  <>
  command "hackage" (Hackage <$> info hackageCommand (progDesc "Hackage related commands"))
  where
    bootstrapDesc = "Bootstrap an existing Cabal project in the current directory"

cliLogLevel ::
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  LogLevel
cliLogLevel verbose debug trace quiet
  | trace = LogTrace
  | debug = LogDebug
  | verbose = LogVerbose
  | quiet = LogError
  | otherwise = LogInfo

globalParser ::
  Path Abs Dir ->
  Parser GlobalOptions
globalParser realCwd = do
  verbose <- switch (long "verbose" <> short 'v' <> help "Verbose output")
  debug <- switch (long "debug" <> help "Debug output")
  trace <- switch (long "trace" <> help "Trace debug output")
  quiet <- switch (long "quiet" <> help "Suppress info output")
  cabalVerbose <- switch (long "cabal-verbose" <> help "Verbose output from Cabal")
  cwd <- strOption (long "cwd" <> help "Force a different working directory" <> value (PathConcrete (Abs realCwd)))
  rootOverride <- rootParser
  output <- option outputFormatOption (long "output" <> help formatHelp <> value OutputNone <> metavar "format")
  target <- option outputTargetOption (long "target" <> help targetHelp <> value OutputDefault <> metavar "target")
  pure GlobalOptions {root = fromMaybe cwd rootOverride, logLevel = cliLogLevel verbose debug trace quiet, ..}
  where
    formatHelp = "Result output format, if commands support it"
    targetHelp = "Force output to a file or stdout"

appParser ::
  Path Abs Dir ->
  Parser Options
appParser cwd =
  Options <$> globalParser cwd <*> hsubparser commands

parseCli ::
  IO Options
parseCli = do
  realCwd <- getCurrentDir
  customExecParser parserPrefs (info (appParser realCwd <**> helper) (desc <> forwardOptions))
  where
    parserPrefs =
      prefs (showHelpOnEmpty <> showHelpOnError <> subparserInline)
    desc =
      fullDesc <> header "Tools for maintaining Hix projects"
