module Hix.Managed.Maint.MaintPlan where

import qualified Data.Set as Set
import Exon (exon)

import Hix.Class.Map (nInsert, nTransformMulti, nViaA, (!?))
import Hix.Data.Dep (Dep (..))
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage (..), PackageName)
import Hix.Managed.Data.MaintContext (MaintContext (..), MaintPackage (..))
import Hix.Managed.Data.ManagedPackage (ManagedPackage (..))
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Maint.Data.MaintPlan (MaintPlan (..))
import Hix.Managed.Maint.Data.MaintTarget (MaintTarget (..))
import Hix.Managed.Report (plural)
import Hix.Monad (eitherClient, noteClient)
import Hix.Pretty (showPL)

envForTarget :: Packages EnvName -> LocalPackage -> M EnvName
envForTarget envs package =
  noteClient noTargetEnv (envs !? package)
  where
    noTargetEnv = [exon|The package '##{package}' is not a target of any environment.|]

restrictTargets :: NonEmpty PackageName -> Packages MaintPackage -> M (Packages MaintPackage)
restrictTargets specified packages =
  eitherClient (first err (foldr step (Right []) locals))
  where
    step name z
      | Just mp <- packages !? name = nInsert name mp <$> z
      | otherwise = Left (Set.insert name (fromLeft [] z))

    err unknown = [exon|Unknown package#{plural (length unknown)}: #{showPL unknown}|]

    locals = LocalPackage <$> specified

maintPlan ::
  MaintContext ->
  Maybe (NonEmpty PackageName) ->
  M MaintPlan
maintPlan MaintContext {packages, envs} specified = do
  selected <- maybe pure restrictTargets specified packages
  targets <- nViaA (traverse target) selected
  pure MaintPlan {targets}
  where
    target MaintPackage {package = ManagedPackage {name, version, deps}, ..} = do
      env <- envForTarget targetEnvs name
      pure MaintTarget {package = name, version, branch = Nothing, deps = Set.fromList ((.package) <$> deps), ..}

    targetEnvs = flip nTransformMulti envs \ envName targets -> [(t, envName) | t <- targets]
