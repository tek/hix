module Hix.Managed.Cabal.Sdist where

import Control.Monad.Trans.Reader (ask)
import Distribution.Client.CmdSdist (OutputFormat (TarGzArchive), packageToSdist)
import Distribution.Client.Types (PackageLocation (LocalUnpackedPackage))
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Exon (exon)
import Path (Abs, Dir, File, Path, parseRelFile, toFilePath, (</>))

import Hix.Data.Monad (AppResources (..), M (M))
import qualified Hix.Data.PackageId as PackageId
import Hix.Data.PackageId (PackageId (..))
import Hix.Managed.Cabal.PackageDescription (parseCabalFile)
import Hix.Managed.Cabal.Resources (cabalVerbosity)
import Hix.Monad (noteFatal)
import Hix.Pretty (showP)

sourceDistribution ::
  Path Abs Dir ->
  PackageId ->
  M (Path Abs File)
sourceDistribution root package = do
  verbosity <- cabalVerbosity
  AppResources {tmp} <- M ask
  cabalFilename <- parseFilename "cabal" cabalFileString
  filename <- parseFilename "sdist output" fileString
  srcpkgDescription <- parseCabalFile (root </> cabalFilename)
  let outfile = tmp </> filename
      sourcePkg =
        SourcePackage {
          srcpkgPackageId = PackageId.toCabal package,
          srcpkgDescription,
          srcpkgSource = LocalUnpackedPackage (toFilePath root),
          srcpkgDescrOverride = Nothing
        }
  liftIO $ packageToSdist verbosity (toFilePath root) TarGzArchive (toFilePath outfile) sourcePkg
  pure outfile
  where
    cabalFileString = [exon|#{showP package.name}.cabal|]
    fileString = [exon|#{showP package}.tar.gz|]
    parseFilename desc s = noteFatal [exon|Couldn't parse #{desc} file name: #{toText s}|] (parseRelFile s)
