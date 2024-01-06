module Hix.Managed.ProjectContextProto where

import Exon (exon)

import Hix.Class.Map (nBy, nKeys, nKeysSet, nMap, nTo, (!!), (!?))
import Hix.Data.EnvName (EnvName)
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
import Hix.Monad (clientError, noteClient)

validateQuery ::
  Packages ManagedPackage ->
  RawQuery ->
  M (Maybe (NonEmpty MutableDep))
validateQuery packages (RawQuery deps) =
  nonEmpty <$> traverse check deps
  where
    check dep | Just m <- mutables !! dep = pure m
              | otherwise = clientError [exon|'##{dep}' is not a dependency of any package.|]

    mutables :: Deps MutableDep
    mutables = nBy mutablesSet depName
    mutablesSet = mconcat (nTo packages \ _ ManagedPackage {mutable} -> nKeysSet mutable)

noEnvs :: Text
noEnvs =
  [exon|The flake config contains no managed envs.
Most likely this means that you ran the CLI directly.
Please use one of the flake apps '.#bump', .#lower.init', '.#lower.optimize' or '.#lower.stabilize'.|]

unknownEnv :: EnvName -> Text
unknownEnv name =
  [exon|You requested to update the env '##{name}', but it is not present in the managed deps config.
Maybe this env is not enabled for managed dependencies.|]

selectEnvs ::
  Envs (Either EnvDeps EnvContext) ->
  [EnvName] ->
  M (NonEmpty (Either EnvName EnvContext))
selectEnvs envs specified =
  traverse valid =<< noteClient noEnvs (nonEmpty specified <|> nonEmpty (nKeys envs))
  where
    valid env = noteClient (unknownEnv env) (first (const env) <$> envs !? env)

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
  let envDeps = nMap (either id (.deps)) contexts
  state <- ProjectStateProto.validateProjectState opts packages envDeps proto.state
  envTargets <- selectEnvs contexts opts.envs
  pure (projectContext opts.build state packages envTargets)
  where
    packages = ManagedPackageProto.validate proto.packages
