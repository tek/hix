module Hix.Data.Dep where

import Data.Aeson (FromJSON (parseJSON), Value (Object, String), (.:))
import Data.Aeson.Types (Parser)
import Distribution.Package (mainLibSet)
import Distribution.Pretty (Pretty, pretty)
import Distribution.Types.Dependency (Dependency (Dependency))
import Distribution.Version (VersionRange, thisVersion)
import Exon (exon)

import Hix.CabalParsec (unsafeParsec)
import Hix.Data.Json (aesonParsec, jsonParsec)
import qualified Hix.Data.PackageName as PackageName
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version)

data Dep =
  Dep {
    package :: PackageName,
    version :: VersionRange
  }
  deriving stock (Eq, Show, Generic)

toCabal :: Dep -> Dependency
toCabal Dep {..} =
  Dependency (PackageName.toCabal package) version mainLibSet

fromCabal :: Dependency -> Dep
fromCabal (Dependency (PackageName.fromCabal -> package) version _) =
  Dep {package, version}

instance Pretty Dep where
  pretty = pretty . toCabal

mkDep :: PackageName -> VersionRange -> Dep
mkDep package version = Dep {..}

jsonParseCabal :: String -> Parser Dep
jsonParseCabal v = fromCabal <$> aesonParsec v

renderDep :: Dep -> Text
renderDep = show . pretty

thisVersionDep :: PackageName -> Version -> Dep
thisVersionDep package version =
  mkDep package (thisVersion version)

instance FromJSON Dep where
  parseJSON = \case
    String s ->
      jsonParseCabal (toString s)
    Object o -> do
      fromName <- jsonParseCabal =<< o .: "name"
      version <- (jsonParsec <$> o .: "version") <|> pure fromName.version
      pure fromName {version}
    v ->
      fail [exon|Invalid dependency format: #{show v}|]

withVersion :: VersionRange -> Dep -> Dep
withVersion version dep = dep {version}

unsafeDep :: String -> Dep
unsafeDep = fromCabal . unsafeParsec
