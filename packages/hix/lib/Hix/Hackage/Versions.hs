module Hix.Hackage.Versions where

import qualified Data.Set as Set
import Distribution.Parsec (eitherParsec)
import Exon (exon)

import Hix.Data.Monad (M)
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version)
import Hix.Hackage (HackageVersions (HackageVersions), hackageGet)
import Hix.Managed.Handlers.HackageClient (HackageClient, HackageError (..), HackageResponse (HackageResponseJson))
import Hix.Monad (fatalError, appContextVerbose)

parseVersion :: String -> Either (String, String) Version
parseVersion s = first (s,) (eitherParsec s)

parseVersions :: HackageVersions -> Either Text (Set Version)
parseVersions (HackageVersions versions) =
  bimap parseError Set.fromList (traverse parseVersion versions)
  where
    parseError (v, err) = toText [exon|Version '#{v}' has invalid format (#{err})|]

allClientVersions :: NonEmpty HackageClient -> PackageName -> M (Set Version)
allClientVersions clients pkg = do
  results <- for clients \ client -> hackageGet parseVersions client path HackageResponseJson
  appContextVerbose [exon|fetching versions for ##{pkg}|] do
    sconcat <$> traverse (leftA check) results
  where
    path = [exon|/package/##{pkg}/preferred|]

    check = \case
      HackageNotFound -> pure mempty
      HackageFatal err -> fatalError err
      HackageParseError err -> fatalError err

versionsHackage :: NonEmpty HackageClient -> PackageName -> M [Version]
versionsHackage clients pkg =
  Set.toDescList <$> allClientVersions clients pkg

latestVersionHackage :: NonEmpty HackageClient -> PackageName -> M (Maybe Version)
latestVersionHackage client pkg =
  Set.lookupMax <$> allClientVersions client pkg
