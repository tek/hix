module Hix.Managed.Release.Data.TargetSpec where

import Distribution.Parsec (eitherParsec)

import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version)

-- | A release target specified via CLI, either just a package name (for selection) or a package ID with an explicit
-- version to release.
data TargetSpec =
  -- | Only select release targets by package name, version determined by other means (@--version@, UI, etc.).
  TargetName PackageName
  |
  -- | Release this package with an explicit version.
  TargetExplicit PackageId
  deriving stock (Eq, Show)

instance IsString TargetSpec where
  fromString s =
    case eitherParsec s of
      Right pid -> TargetExplicit pid
      Left _ -> TargetName (fromString s)

targetSpecName :: TargetSpec -> PackageName
targetSpecName = \case
  TargetName name -> name
  TargetExplicit PackageId {name} -> name

targetSpecVersion :: TargetSpec -> Maybe Version
targetSpecVersion = \case
  TargetName _ -> Nothing
  TargetExplicit PackageId {version} -> Just version
