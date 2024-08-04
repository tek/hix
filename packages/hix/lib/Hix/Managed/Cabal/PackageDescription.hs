module Hix.Managed.Cabal.PackageDescription where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import Distribution.Fields (runParseResult)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec (showPError)
import Exon (exon)
import Path (Abs, File, Path, toFilePath)

import Hix.Data.Monad (M)
import Hix.Monad (eitherFatal, tryIOM)

parseCabal :: String -> ByteString -> Either Text GenericPackageDescription
parseCabal source content =
  first formatError $
  snd $
  runParseResult $
  parseGenericPackageDescription content
  where
    formatError (_, errs) =
      [exon|Cabal file invalid: #{Text.unlines (toList (toText . showPError source <$> errs))}|]

parseCabalFile :: Path Abs File -> M GenericPackageDescription
parseCabalFile path = do
  content <- tryIOM $ ByteString.readFile (toFilePath path)
  eitherFatal (parseCabal (toFilePath path) content)
