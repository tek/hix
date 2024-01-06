module Hix.Managed.EnvContext where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Exon (exon)

import Hix.Class.Map (nKeysSet, nMapWithKey)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (ProjectOptions)
import Hix.Data.PackageName (LocalPackage)
import qualified Hix.Managed.Data.EnvConfig
import Hix.Managed.Data.EnvConfig (EnvConfig)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext (EnvContext), EnvDeps (EnvDeps))
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import Hix.Managed.Data.Mutable (MutableDep, mutRelax)
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

envContext ::
  ProjectOptions ->
  Packages ManagedPackage ->
  Maybe (NonEmpty MutableDep) ->
  EnvName ->
  EnvConfig ->
  Either EnvDeps EnvContext
envContext opts packages querySpec env envConfig =
  maybeToRight deps (create <$> nonEmpty envQuery)
  where
    create query = EnvContext {ghc = envConfig.ghc, query, deps, ..}

    deps = EnvDeps {local, mutable}

    solverBounds | opts.mergeBounds = mutRelax mutableDeps
                 | otherwise = mempty

    envQuery = maybe (toList mutable) (NonEmpty.filter (flip Set.member mutable)) querySpec

    local = nKeysSet localDeps

    mutable = nKeysSet mutableDeps

    (targets, localDeps, mutableDeps) = ManagedPackage.forTargets packages envConfig.targets

envContexts ::
  ProjectOptions ->
  Packages ManagedPackage ->
  Envs EnvConfig ->
  Maybe (NonEmpty MutableDep) ->
  M (Envs (Either EnvDeps EnvContext))
envContexts opts packages envConfigs query = do
  pure (nMapWithKey (envContext opts packages query) envConfigs)
