module Hix.Managed.Env where

import qualified Data.Text as Text

import Hix.Data.EnvName (EnvName (..))
import Hix.Data.VersionBounds (Bound (..))

inferredBound :: EnvName -> Maybe Bound
inferredBound (EnvName name) =
  if Text.isPrefixOf "latest" name || Text.isPrefixOf "upper" name
  then Just BoundUpper
  else if Text.isPrefixOf "lower" name
  then Just BoundLower
  else Nothing
