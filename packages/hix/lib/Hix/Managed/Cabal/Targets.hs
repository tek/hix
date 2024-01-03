module Hix.Managed.Cabal.Targets where

import Distribution.Client.Dependency (
  PackagePreference (PackageInstalledPreference, PackageVersionPreference),
  PackageProperty (PackagePropertyVersion),
  PackageSpecifier (NamedPackage),
  )
import Distribution.Solver.Types.InstalledPreference (InstalledPreference (PreferInstalled, PreferOldest))
import Distribution.Version (orEarlierVersion, orLaterVersion)

import Hix.Class.Map (nToWith)
import qualified Hix.Data.PackageName as PackageName
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (VersionRange)
import Hix.Data.VersionBounds (Bound (BoundLower, BoundUpper), VersionBounds, maybeRange)
import Hix.Managed.Cabal.Data.SolveTarget (SolveTarget (..))
import qualified Hix.Managed.Data.Constraints
import Hix.Managed.Data.Constraints (EnvConstraints, MutationConstraints (MutationConstraints))

reifyBounds :: VersionBounds -> Maybe VersionRange
reifyBounds =
  maybeRange \case
    BoundLower -> orLaterVersion
    BoundUpper -> orEarlierVersion

solveTarget ::
  PackageName ->
  MutationConstraints ->
  SolveTarget
solveTarget package MutationConstraints {mutation, oldest, installed, prefer, force} =
  SolveTarget {dep, prefs}
  where
    dep = NamedPackage cabalName (PackagePropertyVersion <$> ranges)

    ranges = catMaybes [reifyBounds mutation, force]

    prefs = catMaybes [prefVersion <$> prefer, flag prefOldest oldest, flag prefInstalled installed]

    flag v = \case
      Just True -> Just v
      _ -> Nothing

    prefVersion = PackageVersionPreference cabalName
    prefOldest = PackageInstalledPreference cabalName PreferOldest
    prefInstalled = PackageInstalledPreference cabalName PreferInstalled

    cabalName = PackageName.toCabal package

solveTargets :: EnvConstraints -> [SolveTarget]
solveTargets = nToWith solveTarget
