module Hix.Managed.Cabal.Data.InstalledOverrides where

import Hix.Data.PackageId (PackageId)

-- | Package IDs that are present in the 'GhcDb' provided to the Cabal resources initialization, but that should not be
-- treated as installed because they had to be added as overrides when resolving the package set.
-- This is necessary to avoid having to resolve these overrides again when building mutations.
newtype InstalledOverrides =
  InstalledOverrides { ids :: Set PackageId }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)
