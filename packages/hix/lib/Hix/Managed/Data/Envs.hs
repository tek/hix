module Hix.Managed.Data.Envs where

import Data.Aeson (FromJSON)
import Distribution.Pretty (Pretty (pretty))
import GHC.Exts (IsList)

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Class.Map (LookupMonoid, NMap, nPretty1)
import Hix.Data.EnvName (EnvName)

newtype Envs a =
  Envs (Map EnvName a)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList, EncodeNix)

instance NMap (Envs a) EnvName a LookupMonoid where

instance Pretty a => Pretty (Envs a) where
  pretty = nPretty1
