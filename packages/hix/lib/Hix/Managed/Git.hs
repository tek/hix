module Hix.Managed.Git where

import Control.Lens ((<>:~))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Distribution.Compat.CharParsing (string, try)
import Distribution.Parsec (Parsec (parsec))
import Distribution.Pretty (Pretty, pretty)
import Exon (exon)
import GHC.IsList (IsList (..))
import Path (Abs, Dir, File, Path, toFilePath)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (proc, readProcess, setEnv, setWorkingDir)
import Text.PrettyPrint (text)

import Hix.Class.Map (LookupMaybe, NMap, nToWith)
import qualified Hix.Color as Color
import Hix.Data.Monad (M)
import Hix.Data.PackageId (PackageId (..))
import qualified Hix.Data.PackageName as PackageName
import Hix.Data.PackageName (LocalPackage (..))
import Hix.Data.Version (Version)
import Hix.Error (pathText)
import Hix.Managed.Data.GitConfig (GitConfig (..), GitEnvVars (..), gitConfigHermetic)
import Hix.Monad (appContextDebug, fatalError, tryIOM, withTempDir)
import Hix.Pretty (prettyNt)

data ProcessResult =
  ProcessResult {
    code :: ExitCode,
    stdout :: [Text],
    stderr :: [Text]
  }
  deriving stock (Eq, Show, Generic)

newtype EnvKey =
  EnvKey Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, ToString, Ord)

instance Pretty EnvKey where
  pretty = text . toString

newtype EnvVal =
  EnvVal Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, ToString, Ord)

instance Pretty EnvVal where
  pretty = text . toString

newtype EnvVars =
  EnvVars (Map EnvKey EnvVal)
  deriving stock (Eq, Show)
  deriving newtype (IsList, Semigroup, Monoid)

instance NMap EnvVars EnvKey EnvVal LookupMaybe where

data GitCmd =
  GitCmd {
    repo :: Path Abs Dir,
    args :: [Text]
  }
  deriving stock (Eq, Show, Generic)

data GitResult =
  GitSuccess { stdout :: [Text] }
  |
  GitFailure { code :: Int, stdout :: [Text], stderr :: [Text] }
  deriving stock (Eq, Show)

gitProcess :: Maybe (Path Abs File) -> Maybe EnvVars -> GitCmd -> M ProcessResult
gitProcess exe env GitCmd {repo, args} =
  appContextDebug [exon|running process #{Color.shellCommand cmdline} with #{envInfo} env|] do
    result <$> tryIOM (readProcess conf)
  where
    conf =
      maybe id (setEnv . envString) env $
      setWorkingDir (toFilePath repo) $
      proc (maybe "git" toFilePath exe) (toString <$> args)

    result (code, stdout, stderr) =
      ProcessResult {
        code,
        stdout = Text.lines (decodeUtf8 stdout),
        stderr = Text.lines (decodeUtf8 stderr)
      }

    envString = nToWith \ k v -> (toString k, toString v)

    cmdline = [exon|git #{Text.unwords args}|]

    envInfo =
      if isJust env
      then "custom"
      else "global"

gitExec :: Maybe (Path Abs File) -> Maybe EnvVars -> GitCmd -> M GitResult
gitExec exe env cmd =
  gitProcess exe env cmd <&> \case
    ProcessResult {code, stdout, stderr}
      | ExitSuccess <- code
      -> GitSuccess stdout
      | ExitFailure number <- code
      -> GitFailure {code = number, stdout, stderr}

data GitBackend =
  GitBackend {
    exec :: GitCmd -> M GitResult
  }

data GitEnv =
  GitEnv {
    backend :: GitBackend,
    repo :: Path Abs Dir
  }

newtype GitApi api =
  GitApi { run :: ∀ a . Path Abs Dir -> (api -> M a) -> M a }

runGitApi ::
  GitApi api ->
  Path Abs Dir ->
  Text ->
  (api -> M a) ->
  M a
runGitApi (GitApi run) repo ctx use =
  run repo \ api ->
    appContextDebug [exon|operating on a git repo (#{ctx}) at #{Color.path (pathText repo)}|] do
      use api

gitCmdResult :: GitEnv -> [Text] -> M GitResult
gitCmdResult GitEnv {backend, repo} args =
  backend.exec GitCmd {repo, args}

gitCmd' :: GitEnv -> [Text] -> M (Either ([Text], [Text]) [Text])
gitCmd' env args = do
  gitCmdResult env args <&> \case
    GitSuccess {stdout} -> Right stdout
    GitFailure {stdout, stderr} -> Left (stdout, stderr)

gitError ::
  [Text] ->
  [Text] ->
  [Text] ->
  Text
gitError args stdout stderr =
  [exon|Git command failed: git #{Text.unwords args}
stdout:
#{Text.intercalate "\n" stdout}
stderr:
#{Text.intercalate "\n" stderr}|]

gitCmd :: GitEnv -> [Text] -> M [Text]
gitCmd env args =
  gitCmd' env args >>= \case
    Right stdout ->
      pure stdout
    Left (stderr, stdout) ->
      fatalError (gitError args stdout stderr)

gitCmd_ :: GitEnv -> [Text] -> M ()
gitCmd_ env = void . gitCmd env

data GitNative =
  GitNative {
    cmdResult :: [Text] -> M GitResult,
    cmd' :: [Text] -> M (Either ([Text], [Text]) [Text]),
    cmd :: [Text] -> M [Text],
    cmd_ :: [Text] -> M (),
    repo :: Path Abs Dir
  }

gitNative :: GitEnv -> GitNative
gitNative env =
  GitNative {
    cmdResult = gitCmdResult env,
    cmd' = gitCmd' env,
    cmd = gitCmd env,
    cmd_ = gitCmd_ env,
    repo = env.repo
  }

hermeticEnvVars :: Path Abs Dir -> EnvVars
hermeticEnvVars home =
  [
    ("HOME", coerce (pathText home)),
    ("GIT_CONFIG_NOSYSTEM", "1"),
    ("GIT_AUTHOR_NAME", "hix"),
    ("GIT_AUTHOR_EMAIL", "hix-bot@github.com"),
    ("GIT_COMMITTER_NAME", "hix"),
    ("GIT_COMMITTER_EMAIL", "hix-bot@github.com")
  ]

withGitEnvVars :: GitEnvVars -> (Maybe EnvVars -> M a) -> M a
withGitEnvVars envVars f = case envVars of
  GitEnvGlobal -> f Nothing
  GitEnvHermetic -> withTempDir "git-home" \ home -> f (Just (hermeticEnvVars home))
  GitEnvExplicit vars -> f (Just (explicit vars))
  where
    explicit vars = EnvVars (fromList [(EnvKey k, EnvVal v) | (k, v) <- Map.toList vars])

withGitBackend :: GitConfig -> (GitBackend -> M a) -> M a
withGitBackend config f = case config of
  GitConfigGlobal -> f GitBackend {exec = gitExec Nothing Nothing}
  GitConfig {executable, hooks, env} ->
    withGitEnvVars (fromMaybe GitEnvHermetic env) \ vars ->
      f GitBackend {exec = gitExec executable vars . addHooksArgs hooks}
  where
    addHooksArgs enable =
      if fromMaybe False enable
      then id
      else #args <>:~ ["-c", "core.hooksPath=/dev/null"]

-- | Create a 'GitApi' from a 'GitConfig'.
-- For 'GitEnvGlobal', inherits the process environment.
-- For hermetic variants, creates a temp HOME directory.
gitApiFromConfigM :: Maybe GitConfig -> (GitNative -> M api) -> GitApi api
gitApiFromConfigM config consApi =
  GitApi \ repo f ->
    withGitBackend (fromMaybe gitConfigHermetic config) \ backend ->
      f =<< consApi (gitNative GitEnv {backend, repo})

gitApiGlobal :: (GitNative -> M api) -> GitApi api
gitApiGlobal mkApi = gitApiFromConfigM (Just GitConfigGlobal) mkApi

gitApiHermetic :: (GitNative -> M api) -> GitApi api
gitApiHermetic mkApi = gitApiFromConfigM (Just gitConfigHermetic) mkApi

runGitNativeGlobal :: Path Abs Dir -> Text -> (GitNative -> M a) -> M a
runGitNativeGlobal = runGitApi (gitApiGlobal pure)

runGitNativeHermetic :: Path Abs Dir -> Text -> (GitNative -> M a) -> M a
runGitNativeHermetic = runGitApi (gitApiHermetic pure)

data Tag =
  Tag {
    package :: Maybe LocalPackage,
    version :: Version
  }
  deriving stock (Eq, Show, Generic)

instance Ord Tag where
  compare Tag {package = pl, version = vl} Tag {package = pr, version = vr} =
    compare pl pr <> compare vl vr

instance Parsec Tag where
  parsec =
    try (withPackage <$> parsec) <|> (Tag Nothing <$> parsec)
    where
      withPackage PackageId {..} = Tag (Just (LocalPackage name)) version

instance Pretty Tag where
  pretty Tag {package, version}
    | Just p <- package
    = [exon|#{pretty p}-#{pretty version}|]
    | otherwise
    = pretty version

-- TODO rename or refactor this.
data MaintBranch =
  MaintBranch {
    package :: LocalPackage,
    version :: Version
  }
  deriving stock (Eq, Show, Generic)

instance Parsec MaintBranch where
  parsec = do
    string "release/"
    package <- LocalPackage . PackageName.fromCabal <$> parsec
    string "/"
    version <- parsec
    pure MaintBranch {package = package, version}

instance Pretty MaintBranch where
  pretty MaintBranch {package = LocalPackage name, ..} =
    pretty PackageId {..}

newtype BranchName =
  BranchName Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, ToJSON, FromJSON, Semigroup, Monoid)

instance Pretty BranchName where
  pretty = prettyNt
