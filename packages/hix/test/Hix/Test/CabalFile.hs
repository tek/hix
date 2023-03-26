module Hix.Test.CabalFile where

import Distribution.Fields.ParseResult (parseString)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Verbosity (silent)
import Exon (exon)

testCabal :: ByteString
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
      NoApplicativeDo
  ghc-options: -Wall -Wunused-imports
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

testPackage :: IO (GenericPackageDescription)
testPackage =
  parseString parseGenericPackageDescription silent "hix" testCabal

testCabalNoPrelude :: ByteString
testCabalNoPrelude =
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
      NoApplicativeDo
  ghc-options: -Wall -Wunused-imports
  build-depends:
      Cabal
    , base >=4.12 && <5
  default-language: Haskell2010
|]

testPackageNoPrelude :: IO (GenericPackageDescription)
testPackageNoPrelude =
  parseString parseGenericPackageDescription silent "hix" testCabalNoPrelude
