{-# options_ghc -Wno-orphans #-}

module Hix.Test.Managed.UnsafeIsString where

import Distribution.Version (VersionRange)

import Hix.CabalParsec (unsafeParsec)
import qualified Hix.Data.Dep as Dep
import Hix.Data.Dep (Dep)
import qualified Hix.Data.Overrides
import Hix.Data.Overrides (Override (Override))
import qualified Hix.Data.PackageId as PackageId
import Hix.Data.PackageId (PackageId)
import Hix.Data.Version (SourceHash (SourceHash), Version)
import Hix.Pretty (showP)

instance IsString Version where
  fromString = unsafeParsec

instance IsString VersionRange where
  fromString = unsafeParsec

instance IsString PackageId where
  fromString = PackageId.fromCabal . unsafeParsec

instance IsString Dep where
  fromString = Dep.fromCabal . unsafeParsec

instance IsString Override where
  fromString s =
    Override {hash = SourceHash (showP package), version = package.version}
    where
      package = fromString @PackageId s
