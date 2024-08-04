module Hix.Managed.Data.Packages where

import Data.Aeson (FromJSON)
import Distribution.Pretty (Pretty (pretty))
import GHC.Exts (IsList)

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Class.Map (LookupMaybe, LookupMonoid, NMap, nPretty, nPretty1)
import Hix.Data.PackageName (LocalPackage, PackageName)
import Hix.Pretty (HPretty (..), hnPretty)

newtype Packages a =
  Packages (Map LocalPackage a)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList, EncodeNix)

instance NMap (Packages a) LocalPackage a LookupMonoid where

instance Pretty a => Pretty (Packages a) where
  pretty = nPretty1

instance HPretty a => HPretty (Packages a) where
  hpretty = hnPretty

newtype Deps a =
  Deps (Map PackageName a)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList, EncodeNix)

instance NMap (Deps a) PackageName a LookupMaybe where

instance Pretty a => Pretty (Deps a) where
  pretty = nPretty
