module Hix.Managed.Git where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as Text
import Distribution.Compat.CharParsing (string, try)
import Distribution.Parsec (Parsec (parsec))
import Distribution.Pretty (Pretty, pretty)
import Exon (exon)
import GHC.IsList (IsList (..))
import Path (Abs, Dir, Path, toFilePath)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (proc, readProcess, setEnv, setWorkingDir)
import Text.PrettyPrint (text)

import Hix.Class.Map (LookupMaybe, NMap, nTo)
import qualified Hix.Color as Color
import Hix.Data.Monad (M)
import Hix.Data.PackageId (PackageId (..))
import qualified Hix.Data.PackageName as PackageName
import Hix.Data.PackageName (LocalPackage (..))
import Hix.Data.Version (Version)
import Hix.Error (pathText)
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
  deriving stock (Eq, Show)

data GitResult =
  GitSuccess { stdout :: [Text] }
  |
  GitFailure { code :: Int, stdout :: [Text], stderr :: [Text] }
  deriving stock (Eq, Show)

gitProcess :: EnvVars -> GitCmd -> M ProcessResult
gitProcess env GitCmd {repo, args} = do
  appContextDebug [exon|running process #{Color.shellCommand cmdline}|] do
    result <$> tryIOM (readProcess conf)
  where
    conf = setEnv envString $ setWorkingDir (toFilePath repo) (proc "git" (toString <$> args))
    result (code, stdout, stderr) =
      ProcessResult {
        code,
        stdout = Text.lines (decodeUtf8 stdout),
        stderr = Text.lines (decodeUtf8 stderr)
      }

    envString = nTo env \ k v -> (toString k, toString v)

    cmdline = [exon|git #{Text.unwords args}|]

gitExec :: EnvVars -> GitCmd -> M GitResult
gitExec env cmd =
  gitProcess env cmd <&> \case
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
  run repo \ api -> appContextDebug [exon|operating on a git repo (#{ctx}) at '#{pathText repo}'|] (use api)

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

gitApiWithNativeEnv :: (GitEnv -> api) -> GitApi api
gitApiWithNativeEnv api =
  GitApi \ repo f -> f (api GitEnv {backend = GitBackend {exec = gitExec mempty}, repo})

gitApi :: (GitNative -> api) -> GitApi api
gitApi api =
  GitApi \ repo f -> f (api (gitNative GitEnv {backend = GitBackend {exec = gitExec mempty}, repo}))

gitApiNative :: GitApi GitNative
gitApiNative = gitApi id

-- TODO allow configuring the user data, especially for CI
gitEnvHermetic :: Path Abs Dir -> Path Abs Dir -> GitEnv
gitEnvHermetic home repo =
  GitEnv {backend = GitBackend {exec = gitExec env}, repo}
  where
    env =
      [
        ("HOME", coerce (pathText home)),
        ("GIT_CONFIG_NOSYSTEM", "1"),
        ("GIT_AUTHOR_NAME", "hix"),
        ("GIT_AUTHOR_EMAIL", "hix@tryp.io"),
        ("GIT_COMMITTER_NAME", "hix"),
        ("GIT_COMMITTER_EMAIL", "hix@tryp.io")
      ]

gitHermetic :: Path Abs Dir -> Path Abs Dir -> GitNative
gitHermetic home repo = gitNative (gitEnvHermetic home repo)

gitApiHermetic :: (GitNative -> api) -> GitApi api
gitApiHermetic consApi =
  GitApi \ repo f ->
    withTempDir "git-home" \ home ->
      f (consApi (gitNative (gitEnvHermetic home repo)))

gitApiNativeHermetic :: GitApi GitNative
gitApiNativeHermetic = gitApiHermetic id

runGitNative :: Path Abs Dir -> Text -> (GitNative -> M a) -> M a
runGitNative = runGitApi gitApiNative

runGitNativeHermetic :: Path Abs Dir -> Text -> (GitNative -> M a) -> M a
runGitNativeHermetic = runGitApi gitApiNativeHermetic

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
