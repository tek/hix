module Hix.Managed.Data.GitConfig where

import Data.Aeson (FromJSON (..), Object, Value (..), withObject, withText, (.:?))
import Data.Aeson.Types (Parser)
import Exon (exon)
import Path (Abs, File, Path)

namedOrObject ::
  String ->
  (Text -> Maybe a) ->
  (Object -> Parser a) ->
  Value ->
  Parser a
namedOrObject desc named full v =
  leftA invalidName =<< (withText desc textValue v <|> withObject desc (fmap Right . full) v)
  where
    textValue t = pure (maybeToRight t (named t))

    invalidName name =
      fail [exon|Unknown git config preset '#{toString name}', expected 'global', 'hermetic', or an object|]

-- | Environment variable configuration for git processes.
data GitEnvVars =
  -- | Inherit the process environment without modification.
  GitEnvGlobal
  |
  -- | Clear the environment and set minimal hermetic variables (committer identity, @GIT_CONFIG_NOSYSTEM@).
  GitEnvHermetic
  |
  -- | Clear the environment and set the given variables.
  --
  -- TODO this doesn't set GIT_CONFIG_NOSYSTEM. While users could set that themselves, it might be nice to have a
  -- shorthand in GitConfig.
  GitEnvExplicit (Map Text Text)
  deriving stock (Eq, Show, Generic)

instance FromJSON GitEnvVars where
  parseJSON =
    namedOrObject "GitEnvVars" named (fmap GitEnvExplicit . parseJSON . Object)
    where
      named = \case
        "global" -> Just GitEnvGlobal
        "hermetic" -> Just GitEnvHermetic
        _ -> Nothing

-- | Git process configuration.
data GitConfig =
  -- | Shorthand for enabling hooks and inheriting the global environment.
  GitConfigGlobal
  |
  GitConfig {
    -- | Override the git executable path.
    executable :: Maybe (Path Abs File),
    -- | Whether to run git hooks. Defaults to 'False' when not specified.
    hooks :: Maybe Bool,
    -- | Environment variable strategy. Defaults to 'GitEnvHermetic' when not specified.
    env :: Maybe GitEnvVars
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON GitConfig where
  parseJSON =
    namedOrObject "GitConfig" named full
    where
      named = \case
        "global" -> Just GitConfigGlobal
        "hermetic" -> Just GitConfig {executable = Nothing, hooks = Just False, env = Just GitEnvHermetic}
        _ -> Nothing

      full o = do
        executable <- o .:? "executable"
        hooks <- o .:? "hooks"
        env <- o .:? "env"
        pure GitConfig {..}

gitConfigHermetic :: GitConfig
gitConfigHermetic =
  GitConfig {executable = Nothing, hooks = Just False, env = Just GitEnvHermetic}
