module Hix.Managed.Data.ProjectStateProto where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:?))
import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (hang, ($+$))

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Data.Bounds (Bounds)
import Hix.Data.Json (foldMissing)
import Hix.Data.Overrides (Overrides)
import Hix.Data.Version (Versions)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.Packages (Packages)

data ProjectStateProto =
  ProjectStateProto {
    bounds :: Packages Bounds,
    versions :: Envs Versions,
    initial :: Envs Versions,
    overrides :: Envs Overrides,
    solver :: Envs Overrides,
    resolving :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (EncodeNix)

instance Pretty ProjectStateProto where
  pretty ProjectStateProto {..} =
    hang "bounds:" 2 (pretty bounds) $+$
    hang "versions:" 2 (pretty versions) $+$
    hang "initial:" 2 (pretty initial) $+$
    hang "overrides:" 2 (pretty overrides) $+$
    hang "solver:" 2 (pretty solver)

instance FromJSON ProjectStateProto where
  parseJSON = withObject "ProjectStateProto" \ o -> do
    bounds <- foldMissing o "bounds"
    versions <- foldMissing o "versions"
    initial <- foldMissing o "initial"
    overrides <- foldMissing o "overrides"
    solver <- foldMissing o "solver"
    resolving <- fromMaybe False <$> o .:? "resolving"
    pure ProjectStateProto {..}

instance Default ProjectStateProto where
  def =
    ProjectStateProto {
      bounds = mempty,
      versions = mempty,
      initial = mempty,
      overrides = mempty,
      solver = mempty,
      resolving = False
    }
