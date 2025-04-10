{-# options_ghc -Wno-orphans #-}

module Hix.Test.Managed.UnsafeIsString where

import Distribution.Simple (Dependency)
import Distribution.Version (VersionRange)
import Exon (exon)
import GHC.Exts (IsList (Item, fromList, toList))

import Hix.CabalParsec (unsafeParsec)
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep (Dep), unsafeDep)
import Hix.Data.Overrides (Override, override)
import qualified Hix.Data.PackageId as PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (SourceHash (SourceHash), Version)
import qualified Hix.Data.VersionBounds
import Hix.Data.VersionBounds (VersionBounds (VersionBounds), fromLower, unsafeVersionBoundsFromRange, versionBounds)
import Hix.Managed.Cabal.Data.SourcePackage (SourcePackageId (..))
import Hix.Managed.Data.Constraints (MutationConstraints (mutation))
import Hix.Managed.Data.Mutable (MutableDep, unsafeMutableDep)
import qualified Hix.Managed.Data.MutableId
import Hix.Managed.Data.MutableId (MutableId (MutableId))
import Hix.Pretty (showP)

instance IsString Version where
  fromString = unsafeParsec

instance IsString VersionRange where
  fromString = unsafeParsec

instance IsString VersionBounds where
  fromString =
    unsafeVersionBoundsFromRange . fromString

instance IsList VersionBounds where
  type Item VersionBounds = Version

  fromList = \case
    [] -> VersionBounds {lower = Nothing, upper = Nothing}
    [lower] -> fromLower lower
    [lower, upper] -> versionBounds lower upper
    l -> error [exon|IsList VersionBounds: not two elements or fewer: #{show l}|]

  toList VersionBounds {lower, upper} = maybeToList lower ++ maybeToList upper

instance IsString MutableDep where
  fromString = unsafeMutableDep . fromString

instance IsString PackageId where
  fromString = PackageId.fromCabal . unsafeParsec

instance IsString MutableId where
  fromString s =
    MutableId {name = unsafeMutableDep name, version}
    where
      PackageId {name, version} = fromString s

instance IsString Dep where
  fromString = unsafeDep

instance IsString Dependency where
  fromString = unsafeParsec

instance IsString Override where
  fromString s =
    override package.version (SourceHash (showP package))
    where
      package = fromString @PackageId s

instance IsString (PackageName, MutationConstraints) where
  fromString s =
    (package, mempty {mutation = unsafeVersionBoundsFromRange version})
    where
      Dep {package, version} = fromString s

instance IsList SourcePackageId where
  type Item SourcePackageId = Dep

  fromList s = SourcePackageId {deps = fromList s, description = Nothing}

  toList = (.deps)
