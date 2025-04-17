module Hix.Managed.Cabal.Data.Config where

import Data.Aeson (FromJSON (parseJSON))
import qualified Data.List.NonEmpty as NonEmpty
import Distribution.Pretty (Pretty (pretty))
import Distribution.Verbosity (Verbosity, verbose)
import Path (Abs, Dir, Path)
import qualified Text.PrettyPrint as PrettyPrint
import Text.PrettyPrint (hang, text, (<+>))

import Hix.Data.Monad (M)
import Hix.Managed.Cabal.Data.HackageLocation ()
import Hix.Managed.Cabal.Data.HackageRepo (HackageRepo (..))
import Hix.Managed.Cabal.Data.Packages (GhcPackages)
import Hix.Managed.Cabal.HackageRepo (centralHackage, unsafeCentralHackageFixed)
import Hix.Maybe (fromMaybeA)
import Hix.Monad (fatalError)
import Hix.Pretty (HPretty (..), fieldOr, fieldWith, prettyMap, prettyV)

newtype GhcPath =
  GhcPath (Path Abs Dir)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, HPretty)

data GhcDb =
  GhcDbSystem (Maybe GhcPath)
  |
  GhcDbSynthetic GhcPackages
  deriving stock (Eq, Show, Generic)

instance FromJSON GhcDb where
  parseJSON = fmap GhcDbSystem . parseJSON

instance Pretty GhcDb where
  pretty = \case
    GhcDbSystem (Just path) -> hpretty path
    GhcDbSystem Nothing -> "no GHC"
    GhcDbSynthetic pkgs -> hpretty pkgs

data HackagePurpose =
  ForVersions
  |
  ForSolver
  |
  ForPublish
  deriving stock (Eq, Show)

describePurpose :: IsString a => HackagePurpose -> a
describePurpose = \case
  ForVersions -> "fetching versions"
  ForSolver -> "resolving dependencies"
  ForPublish -> "publishing packages"

data CabalConfig =
  CabalConfig {
    hackageMain :: Maybe HackageRepo,
    hackageExtra :: [HackageRepo]
  }
  deriving stock (Eq, Show, Generic)

instance Pretty CabalConfig where
  pretty CabalConfig {..} =
    prettyMap "cabal" [
      fieldOr "main" "default" hackageMain,
      fieldWith "extra" (nonEmpty hackageExtra) prettyV
    ]

instance Default CabalConfig where
  def = CabalConfig {hackageMain = Nothing, hackageExtra = []}

-- | This forces at least the default repo to be available.
-- If this is not desired, the repo should be included with the 'enable' flag unset.
allHackages :: CabalConfig -> NonEmpty HackageRepo
allHackages CabalConfig {..} =
  fromMaybe centralHackage hackageMain :| hackageExtra

hackagesFor :: HackagePurpose -> CabalConfig -> M (NonEmpty HackageRepo)
hackagesFor purpose config =
  fromMaybeA noMatches $
  nonEmpty $
  NonEmpty.filter match repos
  where
    match repo =
      repo.enable && case purpose of
      ForVersions -> True
      ForSolver -> repo.solver
      ForPublish -> repo.publish

    noMatches =
      fatalError $ show $
      hang (text "None of the configured Hackage repos are allowed for" <+> text desc PrettyPrint.<> text ":") 2 $
      prettyV repos

    repos = allHackages config

    desc = describePurpose purpose

withHackageFixed :: CabalConfig -> CabalConfig
withHackageFixed config =
  config {hackageMain = config.hackageMain <|> Just unsafeCentralHackageFixed}

data SolveConfig =
  SolveConfig {
    hackageRepos :: NonEmpty HackageRepo,
    verbosity :: Verbosity,
    ghc :: Maybe GhcPath,
    allowBoot :: Bool,
    cabal :: CabalConfig
  }
  deriving stock (Eq, Show, Generic)

instance Default SolveConfig where
  def =
    SolveConfig {
      hackageRepos = [centralHackage],
      verbosity = verbose,
      ghc = Nothing,
      allowBoot = False,
      cabal = def
    }
