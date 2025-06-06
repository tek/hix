module Hix.Managed.ProjectContextProto where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Exon (exon)

import Hix.Class.Map (nConcat, nGen, nKeys, nKeysSet, nMap, nOver, (!?))
import Hix.Data.Dep (Dep (..))
import Hix.Data.EnvName (EnvName (..))
import Hix.Data.Error (ErrorMessage (Client))
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (ProjectOptions)
import Hix.Data.PackageName (PackageName)
import Hix.Data.VersionBounds (Bound)
import Hix.Managed.Cabal.Config (cabalConfig)
import Hix.Managed.Cabal.Data.Config (CabalConfig)
import Hix.Managed.Data.BuildConfig (BuildConfig)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext, EnvDeps)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.ManagedPackage (ManagedPackage (..), ProjectPackages)
import qualified Hix.Managed.Data.ProjectContext
import Hix.Managed.Data.ProjectContext (ProjectContext (ProjectContext))
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto)
import Hix.Managed.Data.ProjectState (ProjectState)
import Hix.Managed.Data.Query (RawQuery (RawQuery))
import Hix.Managed.EnvContext (envContexts)
import qualified Hix.Managed.ProjectStateProto as ProjectStateProto
import Hix.Monad (fromEither, noteClient)
import Hix.Pretty (showP)

validateQuery ::
  Set PackageName ->
  RawQuery ->
  Either ErrorMessage (Maybe (NonEmpty PackageName))
validateQuery projectDeps (RawQuery query) =
  maybeToLeft (nonEmpty query) (err <$> find invalid query)
  where
    err :: PackageName -> ErrorMessage
    err dep = Client [exon|'##{dep}' is not a dependency of any package.|]

    invalid dep = not (Set.member dep projectDeps)

noEnvs :: Text
noEnvs =
  [exon|The flake config contains no managed envs.
Most likely this means that you ran the CLI directly.
Please use one of the flake apps '.#bump', .#lower.init', '.#lower.optimize' or '.#lower.stabilize'.|]

unknownEnv :: EnvName -> Text
unknownEnv name =
  [exon|You requested to update the env '##{name}', but it is not present in the managed deps config.
Maybe this env is not enabled for managed dependencies.|]

noMatchingBound :: Bool -> Bound -> Text
noMatchingBound selected bound =
  [exon|None of the #{what} are configured to manage #{showP bound} bounds, which are targeted by this app.|]
  where
    what =
      if selected
      then "selected envs"
      else "envs in the flake config"

selectEnvs ::
  Bound ->
  Envs (Either EnvDeps EnvContext) ->
  [EnvName] ->
  M (NonEmpty (Either EnvName EnvContext))
selectEnvs bound envs specified = do
  selected <- traverse valid =<< noteClient noEnvs (nonEmpty specified <|> nonEmpty (nKeys envs))
  noteClient (noMatchingBound (null specified) bound) (nonEmpty (NonEmpty.filter matchBound selected))
  where
    valid env = noteClient (unknownEnv env) (first (const env) <$> envs !? env)

    matchBound = \case
      Left _ -> True
      Right context -> bound == context.bound

projectContext ::
  BuildConfig ->
  ProjectState ->
  ProjectPackages ->
  NonEmpty (Either EnvName EnvContext) ->
  CabalConfig ->
  ProjectContext
projectContext build state packages envs cabal =
  ProjectContext {..}

validate ::
  Bound ->
  ProjectOptions ->
  ProjectContextProto ->
  M ProjectContext
validate bound opts proto = do
  query <- fromEither (validateQuery projectDeps opts.query)
  let contexts = envContexts opts proto.packages proto.envs query
      envDeps = nMap (either id (.deps)) contexts
  state <- ProjectStateProto.validateProjectState opts ranges envDeps proto.state
  envs <- selectEnvs bound contexts opts.envs
  cabal <- cabalConfig proto.hackage opts.cabal
  pure ProjectContext {
    build = opts.build,
    state,
    packages = proto.packages,
    envs,
    cabal
  }
  where
    projectDeps = nConcat ranges (const nKeysSet)

    ranges =
      nOver proto.packages \ ManagedPackage {deps} ->
        nGen deps \ Dep {package = depPackage, ..} ->
          (depPackage, version)

