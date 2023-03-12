{-# language OverloadedRecordDot #-}

module Hix.Test.CabalTest where

import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString as ByteString
import Distribution.PackageDescription (
  BuildInfo (defaultExtensions),
  CondTree (condTreeData),
  GenericPackageDescription (condLibrary),
  Library (libBuildInfo),
  )
import Hedgehog (TestT, (===))
import Hix.Cabal (parseCabal)
import Language.Haskell.Extension (
  Extension (DisableExtension, EnableExtension),
  KnownExtension (AllowAmbiguousTypes, ApplicativeDo),
  )
import Path (relfile, toFilePath, (</>))
import Path.IO (withSystemTempDir)

import Hix.Test.CabalFile (testCabal)

target :: [Extension]
target =
  [EnableExtension AllowAmbiguousTypes, DisableExtension ApplicativeDo]

test_cabal :: TestT IO ()
test_cabal = do
  Right pkg <- liftIO $ withSystemTempDir "hix-unit" \ tmp -> do
    let cabalFile = tmp </> [relfile|test.cabal|]
    ByteString.writeFile (toFilePath cabalFile) testCabal
    runExceptT (parseCabal cabalFile)
  Just target === (pkg.condLibrary <&> \ l -> l.condTreeData.libBuildInfo.defaultExtensions)
