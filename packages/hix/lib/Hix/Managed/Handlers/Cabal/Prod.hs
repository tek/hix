module Hix.Managed.Handlers.Cabal.Prod where

import qualified Data.Map.Strict as Map
import Distribution.Client.Dependency (PackagesPreferenceDefault (PreferAllOldest), setPreferenceDefault)
import qualified Distribution.Client.Types
import Distribution.Client.Types (SourcePackageDb (SourcePackageDb), UnresolvedSourcePackage)
import Distribution.Simple.PackageIndex (deleteSourcePackageId)
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import Distribution.Solver.Types.PackageIndex (PackageIndex)
import Distribution.Version (laterVersion)

import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId
import qualified Hix.Data.PackageId as PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import qualified Hix.Data.PackageName as PackageName
import Hix.Data.PackageName (LocalPackage, PackageName)
import Hix.Managed.Cabal.Data.Config (CabalConfig, GhcDb)
import Hix.Managed.Cabal.Data.InstalledOverrides (InstalledOverrides (..))
import Hix.Managed.Cabal.Data.SolveResources (SolveResources (..))
import Hix.Managed.Cabal.Installed (installedVersion)
import qualified Hix.Managed.Cabal.Resources as SolveResources
import Hix.Managed.Cabal.Solve (solveWithCabal)
import Hix.Managed.Cabal.Sort (sortMutations)
import Hix.Managed.Cabal.Source (sourcePackage)
import Hix.Managed.Data.ManagedPackage (ProjectPackages)
import Hix.Managed.Handlers.Cabal (CabalHandlers (..))
import Hix.Zip (zipApplyL)

handlersWith ::
  (SolveResources -> SolveResources) ->
  CabalConfig ->
  Bool ->
  ProjectPackages ->
  GhcDb ->
  M (CabalHandlers, Set LocalPackage)
handlersWith trans cabalConf oldest packages ghc = do
  (solveResources, localUnavailable) <- first trans <$> SolveResources.acquire packages cabalConf ghc
  let handlers = CabalHandlers {
    solveForVersion = solveWithCabal solveResources {solverParams},
    installedVersion = installedVersion solveResources.installedPkgIndex,
    sourcePackage = sourcePackage solveResources.sourcePkgDb,
    sortMutations = sortMutations solveResources
  }
  pure (handlers, localUnavailable)
  where
    solverParams | oldest = setPreferenceDefault PreferAllOldest
                 | otherwise = id

-- | Remove the given overrides from the @InstalledPackageIndex@.
--
-- The solver overrides are a necessity that allows us to resolve dependencies and start builds with multiple broken
-- packages in the nixpkgs GHC set, but they obscure what is actually installed, so that the same overrides have to be
-- determined again when running mutation builds (which isn't possible except for revisions).
--
-- Therefore, we remove the solver overrides from the installed package DB here, causing Cabal to see only their
-- counterparts from Hackage.
--
-- Note that we do absolutely need the remaining installed packages to be recognized, since we can instruct Cabal to
-- prefer those versions in order to avoid having to use overrides for every dependency (and build all of those
-- packages despite them being available in the cache).
removeInstalledOverrides ::
  InstalledOverrides ->
  SolveResources ->
  SolveResources
removeInstalledOverrides (InstalledOverrides ids) resources =
  resources {
    installedPkgIndex = foldr (deleteSourcePackageId . PackageId.toCabal) resources.installedPkgIndex ids
  }

handlersProd ::
  InstalledOverrides ->
  CabalConfig ->
  Bool ->
  ProjectPackages ->
  GhcDb ->
  M (CabalHandlers, Set LocalPackage)
handlersProd overrides =
  handlersWith (removeInstalledOverrides overrides)

testPackagesBump :: Map PackageName PackageId
testPackagesBump =
  Map.fromList $ zipApplyL (.name) [
    PackageId {name = "aeson", version = [2, 2, 0, 0]},
    PackageId {name = "base", version = [4, 17, 2, 0]},
    PackageId {name = "extra", version = [1, 7, 14]},
    PackageId {name = "th-abstraction", version = [0, 5, 0, 0]},
    PackageId {name = "path", version = [0, 9, 5]},
    PackageId {name = "multi-fail1", version = [0, 1, 0]},
    PackageId {name = "multi-fail2", version = [0, 1, 0]}
  ]

testPackagesMaint :: Map PackageName PackageId
testPackagesMaint =
  Map.fromList $ zipApplyL (.name) [
    PackageId {name = "base", version = [4, 19, 0, 0]},
    PackageId {name = "extra", version = [1, 8]},
    PackageId {name = "semigroups", version = [0, 20]}
  ]

removeLaterVersions ::
  PackageId ->
  PackageIndex UnresolvedSourcePackage ->
  PackageIndex UnresolvedSourcePackage
removeLaterVersions PackageId {..} =
  PackageIndex.deleteDependency (PackageName.toCabal name) (laterVersion version)

testResourcesFor :: Map PackageName PackageId -> SolveResources -> SolveResources
testResourcesFor packages SolveResources {..} =
  SolveResources {sourcePkgDb = removeLaterTestPackageVersions sourcePkgDb, ..}
  where
    removeLaterTestPackageVersions SourcePackageDb {..} =
      SourcePackageDb {
        packageIndex = Map.foldr' removeLaterVersions packageIndex packages,
        ..
      }

handlersTest ::
  CabalConfig ->
  Bool ->
  ProjectPackages ->
  GhcDb ->
  M (CabalHandlers, Set LocalPackage)
handlersTest =
  handlersWith (testResourcesFor testPackagesBump)
