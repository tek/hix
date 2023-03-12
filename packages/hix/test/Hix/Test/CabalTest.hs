{-# language OverloadedRecordDot #-}

module Hix.Test.CabalTest where

import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Text.IO as Text
import Distribution.PackageDescription
import Exon (exon)
import Hedgehog (TestT)
import Hix (parseCabal)
import Path (relfile, toFilePath, (</>))
import Path.IO (withSystemTempDir)

testCabal :: Text
testCabal =
  [exon|cabal-version: 2.2
name: hix
version: 0.1.0.0
common stuff
  default-extensions:
      ScopedTypeVariables
library
  exposed-modules:
      Hix
  hs-source-dirs:
      lib
  default-extensions:
      AllowAmbiguousTypes
      ApplicativeDo
  ghc-options: -Wall
  build-depends:
      Cabal
    , base >=4.12 && <5
  mixins:
      base hiding (Prelude)
    , incipit-base (IncipitBase as Prelude)
    , incipit-base hiding (IncipitBase)
  default-language: Haskell2010
test-suite hix-unit
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs:
      test
  build-depends:
      base >=4.12 && <5
  default-language: Haskell2010
|]

test_cabal :: TestT IO ()
test_cabal =
  liftIO $ withSystemTempDir "hix-unit" \ tmp -> do
    let cabalFile = tmp </> [relfile|test.cabal|]
    Text.writeFile (toFilePath cabalFile) testCabal
    Right pkg <- liftIO (runExceptT (parseCabal cabalFile))
    dbgs (pkg.condLibrary <&> \ l -> l.condTreeData.libBuildInfo.defaultExtensions)
