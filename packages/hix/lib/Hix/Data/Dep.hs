module Hix.Data.Dep where

import Data.Aeson (FromJSON (parseJSON), Value (Object, String), (.:))
import Data.Aeson.Types (Parser, parseEither)
import Distribution.Compat.NonEmptySet (NonEmptySet)
import Distribution.Package (mainLibSet)
import Distribution.PackageDescription (LibraryName, mkPackageName)
import Distribution.Pretty (Pretty, pretty)
import Distribution.Types.Dependency (Dependency (Dependency))
import Distribution.Version (Version, VersionRange, thisVersion)
import Exon (exon)

import Hix.Data.Json (aesonParsec, jsonParsec)
import Hix.Data.Package (PackageName (PackageName), packageNameFromCabal)
import qualified Hix.Data.Version
import Hix.Data.Version (NewVersion (NewVersion))
import Hix.Version (unsafeRange)

data Dep =
  Dep {
    package :: PackageName,
    version :: VersionRange,
    libs :: NonEmptySet LibraryName
  }
  deriving stock (Eq, Show)

toCabal :: Dep -> Dependency
toCabal Dep {package = PackageName package, ..} =
  Dependency (mkPackageName (toString package)) version libs

instance Pretty Dep where
  pretty = pretty . toCabal

mainDep :: PackageName -> VersionRange -> Dep
mainDep package version =
  Dep {libs = mainLibSet, ..}

unsafeDep :: PackageName -> Text -> Dep
unsafeDep package version =
  mainDep package (unsafeRange version)

parseCabal :: String -> Parser Dep
parseCabal v = do
  Dependency (packageNameFromCabal -> package) version libs <- aesonParsec v
  pure Dep {package, version, libs}

unsafeParseCabal :: String -> Dep
unsafeParseCabal v =
  case parseEither parseCabal v of
    Left err -> error [exon|unsafeParseCabal: #{err}|]
    Right dep -> dep

renderDep :: Dep -> Text
renderDep = show . pretty

thisVersionDep :: PackageName -> Version -> Dep
thisVersionDep package version =
  mainDep package (thisVersion version)

instance FromJSON Dep where
  parseJSON = \case
    String s ->
      parseCabal (toString s)
    Object o -> do
      fromName <- parseCabal =<< o .: "name"
      version <- (jsonParsec <$> o .: "version") <|> pure fromName.version
      pure fromName {version}
    v ->
      fail [exon|Invalid dependency format: #{show v}|]

newVersionDep :: NewVersion -> Dep
newVersionDep NewVersion {..} =
  thisVersionDep package version

withVersion :: VersionRange -> Dep -> Dep
withVersion version dep = dep {version}
