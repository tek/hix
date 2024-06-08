module Hix.Managed.Handlers.Cabal.Prod where

import qualified Data.Map.Strict as Map
import Distribution.Client.Dependency (PackagesPreferenceDefault (PreferAllOldest), setPreferenceDefault)
import qualified Distribution.Client.Types
import Distribution.Client.Types (SourcePackageDb (SourcePackageDb), UnresolvedSourcePackage)
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import Distribution.Solver.Types.PackageIndex (PackageIndex)
import Distribution.Version (laterVersion)

import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import qualified Hix.Data.PackageName as PackageName
import Hix.Data.PackageName (PackageName)
import Hix.Managed.Cabal.Data.Config (CabalConfig, GhcDb)
import qualified Hix.Managed.Cabal.Data.SolveResources as SolveResources
import Hix.Managed.Cabal.Data.SolveResources (SolveResources (SolveResources, solverParams))
import Hix.Managed.Cabal.Installed (installedVersion)
import qualified Hix.Managed.Cabal.Resources as SolveResources
import Hix.Managed.Cabal.Solve (solveWithCabal)
import Hix.Managed.Cabal.Sort (sortMutations)
import Hix.Managed.Cabal.Source (sourcePackage)
import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Handlers.Cabal (CabalHandlers (..))
import Hix.Zip (zipApplyL)

handlersWith ::
  (SolveResources -> SolveResources) ->
  CabalConfig ->
  Bool ->
  Packages ManagedPackage ->
  GhcDb ->
  M CabalHandlers
handlersWith trans cabalConf oldest packages ghc = do
  solveResources <- trans <$> SolveResources.acquire packages cabalConf ghc
  pure CabalHandlers {
    solveForVersion = solveWithCabal solveResources {solverParams},
    installedVersion = installedVersion solveResources.installedPkgIndex,
    sourcePackage = sourcePackage solveResources.sourcePkgDb,
    sortMutations = sortMutations solveResources
  }
  where
    solverParams | oldest = setPreferenceDefault PreferAllOldest
                 | otherwise = id

handlersProd ::
  CabalConfig ->
  Bool ->
  Packages ManagedPackage ->
  GhcDb ->
  M CabalHandlers
handlersProd =
  handlersWith id

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

removeLaterVersions ::
  PackageId ->
  PackageIndex UnresolvedSourcePackage ->
  PackageIndex UnresolvedSourcePackage
removeLaterVersions PackageId {..} =
  PackageIndex.deleteDependency (PackageName.toCabal name) (laterVersion version)

testResources :: SolveResources -> SolveResources
testResources SolveResources {..} =
  SolveResources {sourcePkgDb = removeLaterTestPackageVersions sourcePkgDb, ..}
  where
    removeLaterTestPackageVersions SourcePackageDb {..} =
      SourcePackageDb {
        packageIndex = Map.foldr' removeLaterVersions packageIndex testPackagesBump,
        ..
      }

handlersTest ::
  CabalConfig ->
  Bool ->
  Packages ManagedPackage ->
  GhcDb ->
  M CabalHandlers
handlersTest =
  handlersWith testResources
