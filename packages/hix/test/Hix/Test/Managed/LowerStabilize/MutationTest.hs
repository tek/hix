module Hix.Test.Managed.LowerStabilize.MutationTest where

import Data.IORef (readIORef)
import Exon (exon)
import Hedgehog (evalEither, evalMaybe)

import Hix.Data.Error (Error (Fatal))
import qualified Hix.Data.Overrides
import Hix.Data.Overrides (Override (Override))
import Hix.Data.Version (SourceHash (SourceHash), Versions)
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSynthetic))
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages (GhcPackages))
import Hix.Managed.Cabal.Data.SourcePackage (SourcePackages)
import qualified Hix.Managed.Data.EnvConfig
import Hix.Managed.Data.EnvConfig (EnvConfig (EnvConfig))
import Hix.Managed.Data.ManagedPackageProto (ManagedPackageProto, managedPackages)
import Hix.Managed.Data.Packages (Packages)
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (ProjectContextProto))
import qualified Hix.Managed.Data.ProjectStateProto
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (ProjectStateProto))
import Hix.Managed.Data.StageState (BuildStatus (Failure, Success))
import Hix.Managed.Handlers.Lower (LowerHandlers (..))
import qualified Hix.Managed.Handlers.Lower.Test as LowerHandlers
import Hix.Managed.Lower.Stabilize (lowerStabilizeMain)
import Hix.Managed.ProjectContext (withProjectContext)
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import Hix.Test.Utils (UnitTest, runMTest)

packageDb :: SourcePackages
packageDb =
  [
    ("direct1", [
      ([1, 8, 1], []),
      ([1, 9, 1], []),
      ([2, 0, 1], [])
    ]),
    ("direct2", [
      ([1, 8, 1], []),
      ([1, 9, 1], []),
      ([2, 0, 1], [])
    ])
  ]

ghcPackages :: GhcPackages
ghcPackages = GhcPackages {installed = [], available = packageDb}

buildVersions :: Versions -> M BuildStatus
buildVersions = \case
  [("direct1", [2, 0, 1]), ("direct2", [2, 0, 1])] -> pure Success
  [("direct1", [1, 8, 1]), ("direct2", [1, 8, 1])] -> pure Failure
  [("direct1", [1, 8, 1]), ("direct2", [2, 0, 1])] -> pure Failure
  [("direct1", [1, 9, 1]), ("direct2", [2, 0, 1])] -> pure Success
  [("direct1", [1, 9, 1]), ("direct2", [1, 8, 1])] -> pure Failure
  [("direct1", [1, 9, 1]), ("direct2", [1, 9, 1])] -> pure Success
  [] -> throwM (Fatal "Build with no overrides")
  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])

packages :: Packages ManagedPackageProto
packages =
  managedPackages [(("local1", "1.0"), ["direct1", "direct2"])]

initialState :: ProjectStateProto
initialState =
  ProjectStateProto {
    bounds = [
      ("local1", [
        ("direct1", [[1, 8, 1], [2, 1]]),
        ("direct2", [[1, 8, 1], [2, 1]])
      ])
    ],
    versions = [
      ("lower", [
        ("direct1", [1, 8, 1]),
        ("direct2", [1, 8, 1])
      ])
    ],
    overrides = [
      ("lower", [
        ("direct1", Override {version = [1, 8, 1], hash = SourceHash "direct1-1.8.1"}),
        ("direct2", Override {version = [1, 8, 1], hash = SourceHash "direct2-1.8.1"})
      ])
    ],
    initial = [
      ("lower", [
        ("direct1", [2, 0, 1]),
        ("direct2", [2, 0, 1])
      ])
    ],
    resolving = False
  }

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    local1 = {
      direct1 = {
        lower = "1.9.1";
        upper = "2.1";
      };
      direct2 = {
        lower = "1.9.1";
        upper = "2.1";
      };
    };
  };
  versions = {
    lower = {
      direct1 = "1.9.1";
      direct2 = "1.9.1";
    };
  };
  overrides = {
    lower = {
      direct1 = {
        version = "1.9.1";
        hash = "direct1-1.9.1";
      };
      direct2 = {
        version = "1.9.1";
        hash = "direct2-1.9.1";
      };
    };
  };
  initial = {
    lower = {
      direct1 = "2.0.1";
      direct2 = "2.0.1";
    };
  };
  resolving = false;
}
|]

test_lowerStabilizeMutation :: UnitTest
test_lowerStabilizeMutation = do
  (handlers, stateFileRef, _) <- liftIO (LowerHandlers.handlersUnitTest buildVersions ghcPackages)
  let
    proto =
      ProjectContextProto {
        packages,
        state = initialState,
        envs = [("lower", EnvConfig {targets = ["local1"], ghc = GhcDbSynthetic ghcPackages})],
        buildOutputsPrefix = Nothing
      }

  evalEither =<< liftIO do
    runMTest False $ withProjectContext handlers.build def proto \ project ->
      lowerStabilizeMain handlers project
  finalStateFile <- evalMaybe . head =<< liftIO (readIORef stateFileRef)
  eqLines stateFileTarget (renderRootExpr finalStateFile)
