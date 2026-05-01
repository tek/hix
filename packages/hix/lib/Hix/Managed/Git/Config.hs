module Hix.Managed.Git.Config where

import Hix.Data.Monad (M)
import Hix.Data.Options (GitOptions (..))
import Hix.Json (jsonContext)
import Hix.Managed.Data.GitConfig (GitConfig (..))

gitConfig :: GitOptions -> M GitConfig
gitConfig = \case
  GitOptionsGlobal -> pure GitConfigGlobal
  GitOptionsPath spec -> jsonContext spec
