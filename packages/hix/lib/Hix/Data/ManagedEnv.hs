module Hix.Data.ManagedEnv where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (hang, ($+$))

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Data.Bounds (Bounds, TargetBounds)
import Hix.Data.ConfigDeps (ConfigDeps)
import Hix.Data.Overrides (EnvOverrides, Overrides)
import Hix.Data.Package (LocalPackage)

data ManagedState =
  ManagedState {
    bounds :: Bounds,
    overrides :: Overrides
  }
  deriving stock (Eq, Show, Generic)

instance Pretty ManagedState where
  pretty ManagedState {..} =
    hang "bounds:" 2 (pretty bounds) $+$ hang "overrides:" 2 (pretty overrides)

data ManagedEnvState =
  ManagedEnvState {
    bounds :: TargetBounds,
    overrides :: EnvOverrides,
    resolving :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (EncodeNix)

instance Pretty ManagedEnvState where
  pretty ManagedEnvState {..} =
    hang "bounds:" 2 (pretty bounds) $+$ hang "overrides:" 2 (pretty overrides)

instance FromJSON ManagedEnvState where
  parseJSON = withObject "ManagedEnvState" \ o -> do
    deps <- o .: "bounds" <|> pure mempty
    overrides <- o .: "overrides" <|> pure mempty
    pure (ManagedEnvState deps overrides False)

data ManagedLowerEnv =
  ManagedLowerEnv {
    solverBounds :: Bounds
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data ManagedEnv =
  ManagedEnv {
    deps :: ConfigDeps,
    state :: ManagedEnvState,
    lower :: ManagedLowerEnv,
    targets :: [LocalPackage]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
