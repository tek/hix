module Hix.Managed.Data.ProjectState where

import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (hang, ($+$))

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Data.Overrides (Overrides)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.Mutable (MutableBounds, MutableVersions)
import Hix.Managed.Data.Packages (Packages)

data ProjectState =
  ProjectState {
    bounds :: Packages MutableBounds,
    versions :: Envs MutableVersions,
    overrides :: Envs Overrides,
    initial :: Envs MutableVersions,
    resolving :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (EncodeNix)

instance Pretty ProjectState where
  pretty ProjectState {..} =
    hang "bounds:" 2 (pretty bounds) $+$
    hang "versions:" 2 (pretty versions) $+$
    hang "overrides:" 2 (pretty overrides) $+$
    hang "initial:" 2 (pretty initial)

instance Default ProjectState where
  def = ProjectState {bounds = mempty, versions = mempty, overrides = mempty, initial = mempty, resolving = False}
