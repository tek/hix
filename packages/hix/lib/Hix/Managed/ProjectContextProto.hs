module Hix.Managed.ProjectContextProto where

import Exon (exon)

import Hix.Class.Map (nBy, nGenWith, nKeysSet, nTo, (!!))
import Hix.Data.EnvName (EnvName)
import Hix.Data.Error (Error (Client))
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (ProjectOptions)
import Hix.Managed.Data.BuildConfig (BuildConfig)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext, EnvDeps)
import Hix.Managed.Data.Envs (Envs)
import qualified Hix.Managed.Data.ManagedPackage
import Hix.Managed.Data.ManagedPackage (ManagedPackage (ManagedPackage))
import Hix.Managed.Data.Mutable (MutableDep, depName)
import Hix.Managed.Data.Packages (Deps, Packages)
import qualified Hix.Managed.Data.ProjectContext
import Hix.Managed.Data.ProjectContext (ProjectContext (ProjectContext))
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto)
import Hix.Managed.Data.ProjectState (ProjectState)
import Hix.Managed.Data.Query (RawQuery (RawQuery))
import Hix.Managed.EnvContext (envContexts)
import qualified Hix.Managed.ManagedPackageProto as ManagedPackageProto
import qualified Hix.Managed.ProjectStateProto as ProjectStateProto
import Hix.Monad (throwM)

validateQuery ::
  Packages ManagedPackage ->
  RawQuery ->
  M (Maybe (NonEmpty MutableDep))
validateQuery packages (RawQuery deps) =
  nonEmpty <$> traverse check deps
  where
    check dep | Just m <- mutables !! dep = pure m
              | otherwise = throwM (Client [exon|'##{dep}' is not a dependency of any package.|])

    mutables :: Deps MutableDep
    mutables = nBy mutablesSet depName
    mutablesSet = mconcat (nTo packages \ _ ManagedPackage {mutable} -> nKeysSet mutable)

envDepMap :: NonEmpty (Either (EnvName, EnvDeps) EnvContext) -> Envs EnvDeps
envDepMap =
  nGenWith \case
    Right context -> (context.env, context.deps)
    Left a -> a

projectContext ::
  BuildConfig ->
  ProjectState ->
  Packages ManagedPackage ->
  NonEmpty (Either EnvName EnvContext) ->
  ProjectContext
projectContext build state packages envs =
  ProjectContext {
    build,
    state,
    packages,
    envs
  }

validate ::
  ProjectOptions ->
  ProjectContextProto ->
  M ProjectContext
validate opts proto = do
  query <- validateQuery packages opts.query
  contexts <- envContexts opts packages proto.envs query
  let envDeps = envDepMap contexts
  state <- ProjectStateProto.validateProjectState opts packages envDeps proto.state
  pure (projectContext opts.build state packages (first fst <$> contexts))
  where
    packages = ManagedPackageProto.validate proto.packages
