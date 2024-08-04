module Hix.Managed.EnvContext where

import Exon (exon)

import Hix.Class.Map (nKeysSet, nMapWithKey)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (ProjectOptions)
import Hix.Data.PackageName (LocalPackage, PackageName)
import qualified Hix.Managed.Data.EnvConfig
import Hix.Managed.Data.EnvConfig (EnvConfig)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext (EnvContext), EnvDeps (EnvDeps))
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import qualified Hix.Managed.Data.Mutable as Mutable
import Hix.Managed.Data.Mutable (mutRelax)
import Hix.Managed.Data.Packages (Packages)
import qualified Hix.Managed.ManagedPackage as ManagedPackage
import Hix.Monad (clientError)
import Hix.Pretty (showPL)

unknownTargets :: EnvName -> NonEmpty LocalPackage -> M ()
unknownTargets env missing =
  clientError msg
  where
    msg =
      [exon|The flake config for '##{env}' references #{number} in its targets that #{verb} present|]
      <>
      [exon| in the configuration: #{showPL (toList missing)}|]
    (number, verb) | [_] <- missing = ("a package", "isn't")
                   | otherwise = ("several packages", "aren't")

-- | If @envQuery@ is empty, return @Left EnvDeps@, so subsequent steps can be skipped.
-- This happens when the env has no mutable dependencies or if the user restricted the packages to be processed to
-- consist solely of dependencies of other managed sets.
envContext ::
  ProjectOptions ->
  Packages ManagedPackage ->
  Maybe (NonEmpty PackageName) ->
  EnvName ->
  EnvConfig ->
  Either EnvDeps EnvContext
envContext opts packages querySpec env envConfig =
  maybeToRight deps (create <$> nonEmpty envQuery)
  where
    create query = EnvContext {ghc = envConfig.ghc, query, deps, ..}

    deps = EnvDeps {mutable}

    solverBounds | opts.mergeBounds = mutRelax mutableDeps
                 | otherwise = mempty

    -- If the user hasn't specified explicit restrictions, process all dependencies.
    -- Otherwise, filter out dependencies that aren't part of this env.
    envQuery = maybe (toList mutable) (mapMaybe (Mutable.validate mutable) . toList) querySpec

    mutable = nKeysSet mutableDeps

    (targets, mutableDeps) = ManagedPackage.forTargets packages envConfig.targets

-- TODO at some point we might wanna check that the target sets are disjoint
envContexts ::
  ProjectOptions ->
  Packages ManagedPackage ->
  Envs EnvConfig ->
  Maybe (NonEmpty PackageName) ->
  Envs (Either EnvDeps EnvContext)
envContexts opts packages envConfigs query =
  nMapWithKey (envContext opts packages query) envConfigs
