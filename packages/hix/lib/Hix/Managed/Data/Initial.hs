module Hix.Managed.Data.Initial where

import Data.Aeson (FromJSON, ToJSON)
import Distribution.Pretty (Pretty (pretty))

import Hix.Class.EncodeNix (EncodeNix)

newtype Initial a =
  Initial { value :: a }
  deriving stock (Eq, Show, Generic, Functor)
  deriving newtype (Semigroup, Monoid, FromJSON, ToJSON, EncodeNix)

instance Pretty a => Pretty (Initial a) where
  pretty (Initial a) = pretty a

initial :: Initial a -> a
initial = coerce
