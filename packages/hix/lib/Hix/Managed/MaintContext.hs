module Hix.Managed.MaintContext where

import Hix.Class.Map (nTransformMulti)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.VersionBounds (Bound (..))
import Hix.Managed.Data.MaintContext (MaintContext (..), MaintContextProto (..), MaintEnv (..))
import Hix.Managed.Env (inferredBound)

matchBound :: EnvName -> Maybe Bound -> Bool
matchBound name bound =
  Just BoundUpper == (bound <|> inferredBound name)

validate :: MaintContextProto -> M MaintContext
validate MaintContextProto {..} =
  pure MaintContext {..}
  where
    targetEnvs =
      flip nTransformMulti envs \ envName MaintEnv {..} ->
        if matchBound envName managedBound
        then [(t, envName) | t <- targets]
        else []
