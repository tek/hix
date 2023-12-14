module Hix.Data.ManagedEnv where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Distribution.Pretty (Pretty (pretty))
import GHC.Exts (IsList)
import GHC.Generics (Generically (Generically))
import Text.PrettyPrint (hang, ($+$))

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Class.Map (LookupMaybe, NtMap)
import Hix.Data.Bounds (Bounds, TargetBounds)
import Hix.Data.ConfigDeps (ConfigDeps)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Overrides (EnvOverrides, Overrides)
import Hix.Data.Package (LocalPackage)
import Hix.Data.Version (EnvVersions)

data ManagedState =
  ManagedState {
    bounds :: Bounds,
    overrides :: Overrides
  }
  deriving stock (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically ManagedState)

instance Pretty ManagedState where
  pretty ManagedState {..} =
    hang "bounds:" 2 (pretty bounds) $+$ hang "overrides:" 2 (pretty overrides)

data ManagedEnvState =
  ManagedEnvState {
    bounds :: TargetBounds,
    overrides :: EnvOverrides,
    lowerInit :: EnvVersions,
    resolving :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (EncodeNix)

instance Pretty ManagedEnvState where
  pretty ManagedEnvState {..} =
    hang "bounds:" 2 (pretty bounds) $+$ hang "overrides:" 2 (pretty overrides)

instance FromJSON ManagedEnvState where
  parseJSON = withObject "ManagedEnvState" \ o -> do
    bounds <- o .: "bounds" <|> pure mempty
    overrides <- o .: "overrides" <|> pure mempty
    lowerInit <- o .: "lowerInit" <|> pure mempty
    resolving <- o .: "resolving" <|> pure False
    pure ManagedEnvState {..}

data ManagedLowerEnv =
  ManagedLowerEnv {
    solverBounds :: Bounds
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data EnvConfig =
  EnvConfig {
    targets :: [LocalPackage]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

newtype EnvsConfig =
  EnvsConfig (Map EnvName EnvConfig)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList, FromJSON)

instance NtMap EnvsConfig EnvName EnvConfig LookupMaybe where

newtype BuildOutputsPrefix =
  BuildOutputsPrefix Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON)

data ManagedEnv =
  ManagedEnv {
    deps :: ConfigDeps,
    state :: ManagedEnvState,
    lower :: ManagedLowerEnv,
    envs :: EnvsConfig,
    buildOutputsPrefix :: Maybe BuildOutputsPrefix
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
