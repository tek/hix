{-# language CPP #-}

module Hix.Compat (
  parseString,
  readGenericPackageDescription,
) where

#if MIN_VERSION_Cabal(3,8,0)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#endif

#if MIN_VERSION_Cabal(3,8,0)
import Distribution.Simple.PackageDescription (parseString)
#else
import Distribution.Fields.ParseResult (parseString)
#endif
