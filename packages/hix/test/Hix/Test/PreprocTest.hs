module Hix.Test.PreprocTest where

import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Text as Text
import Distribution.PackageDescription (
  CondTree (condTreeData),
  GenericPackageDescription (condLibrary),
  Library (libBuildInfo),
  )
import Exon (exon)
import Hedgehog (evalMaybe, (===))
import Path (absfile)

import Hix.Preproc (fromCabal, preprocessModule)
import Hix.Test.CabalFile (testPackage, testPackageNoPrelude)
import Hix.Test.Utils (UnitTest)

pragmas :: Text
pragmas =
  [exon|{-# options_ghc -Wall -Wunused-imports #-}
{-# language Haskell2010, AllowAmbiguousTypes, NoApplicativeDo #-}|]

header :: Text
header =
  [exon|#{pragmas}
{-# language PackageImports, NoImplicitPrelude #-}
{-# line 1 "/foo/bar/Foo.hs" #-}|]

preprocTestNoPrelude ::
  HasCallStack =>
  ByteString ->
  Text ->
  UnitTest
preprocTestNoPrelude module_ target =
  withFrozenCallStack do
    pkg <- liftIO testPackageNoPrelude
    conf <- evalMaybe (pkg.condLibrary <&> \ l -> fromCabal l.condTreeData.libBuildInfo)
    let result = toLazyByteString (preprocessModule [absfile|/foo/bar/Foo.hs|] conf "Hix_Dummy" module_)
    Text.lines target === Text.lines (decodeUtf8 result)

preprocTest ::
  HasCallStack =>
  ByteString ->
  Text ->
  UnitTest
preprocTest module_ target =
  withFrozenCallStack do
    pkg <- liftIO testPackage
    conf <- evalMaybe (pkg.condLibrary <&> \ l -> fromCabal l.condTreeData.libBuildInfo)
    let result = toLazyByteString (preprocessModule [absfile|/foo/bar/Foo.hs|] conf "Hix_Dummy" module_)
    Text.lines target === Text.lines (decodeUtf8 result)

moduleInsert :: ByteString
moduleInsert =
  [exon|{-# language OverloadedLabels #-}

module Foo (
  moo,

noo,
) where

import Data.Char

f :: Int
f = 1
|]

targetInsert :: Text
targetInsert =
  [exon|#{header}
{-# language OverloadedLabels #-}

module Foo (Hix_Dummy,
  moo,

noo,
) where
import "incipit-base" IncipitBase as Prelude
{-# line 8 "/foo/bar/Foo.hs" #-}

import Data.Char

type Hix_Dummy = Int
{-# line 11 "/foo/bar/Foo.hs" #-}
f :: Int
f = 1
|]

test_preprocInsertPrelude :: UnitTest
test_preprocInsertPrelude =
  preprocTest moduleInsert targetInsert

moduleReplace :: ByteString
moduleReplace =
  [exon|{-# language OverloadedLabels #-}

module Foo (
  moo,

  noo,
  ) where

import Data.Char
import "base" Prelude (sum)
import Data.List
import Prelude hiding (Maybe)
import qualified Prelude as P
import Data.Maybe

f :: Int
f = 1
|]

targetReplace :: Text
targetReplace =
  [exon|#{header}
{-# language OverloadedLabels #-}

module Foo (Hix_Dummy,
  moo,

  noo,
  ) where

import Data.Char
import "incipit-base" IncipitBase (sum)
import Data.List
import "incipit-base" IncipitBase hiding (Maybe)
import qualified "incipit-base" IncipitBase as P
import Data.Maybe

type Hix_Dummy = Int
{-# line 16 "/foo/bar/Foo.hs" #-}
f :: Int
f = 1
|]

test_preprocReplacePrelude :: UnitTest
test_preprocReplacePrelude =
  preprocTest moduleReplace targetReplace

moduleSingleLineModule :: ByteString
moduleSingleLineModule =
  [exon|module Foo where

f :: Int
f = 1
|]

targetSingleLineModule :: Text
targetSingleLineModule =
  [exon|#{header}
module Foo where
import "incipit-base" IncipitBase as Prelude
{-# line 2 "/foo/bar/Foo.hs" #-}

type Hix_Dummy = Int
{-# line 3 "/foo/bar/Foo.hs" #-}
f :: Int
f = 1
|]

test_preprocSingleLineModule :: UnitTest
test_preprocSingleLineModule =
  preprocTest moduleSingleLineModule targetSingleLineModule

moduleSelfExport :: ByteString
moduleSelfExport =
  [exon|module Foo (module Foo) where

f :: Int
f = 1
|]

targetSelfExport :: Text
targetSelfExport =
  [exon|#{header}
module Foo (module Foo) where
import "incipit-base" IncipitBase as Prelude
{-# line 2 "/foo/bar/Foo.hs" #-}

type Hix_Dummy = Int
{-# line 3 "/foo/bar/Foo.hs" #-}
f :: Int
f = 1
|]

test_preprocSelfExport :: UnitTest
test_preprocSelfExport =
  preprocTest moduleSelfExport targetSelfExport

moduleSelfExport2 :: ByteString
moduleSelfExport2 =
  [exon|module Foo (
  module Bar,
  module Foo,
  module Baz,
) where

f :: Int
f = 1
|]

targetSelfExport2 :: Text
targetSelfExport2 =
  [exon|#{header}
module Foo (
  module Bar,
  module Foo,
  module Baz,
) where
import "incipit-base" IncipitBase as Prelude
{-# line 6 "/foo/bar/Foo.hs" #-}

type Hix_Dummy = Int
{-# line 7 "/foo/bar/Foo.hs" #-}
f :: Int
f = 1
|]

test_preprocSelfExport2 :: UnitTest
test_preprocSelfExport2 =
  preprocTest moduleSelfExport2 targetSelfExport2

moduleNoPrelude :: ByteString
moduleNoPrelude =
  [exon|module Main where
import System.Exit (exitSuccess)
|]

targetNoPrelude :: Text
targetNoPrelude =
  [exon|#{pragmas}
{-# line 1 "/foo/bar/Foo.hs" #-}
module Main where
import System.Exit (exitSuccess)
|]

test_preprocNoPrelude :: UnitTest
test_preprocNoPrelude =
  preprocTestNoPrelude moduleNoPrelude targetNoPrelude

modulePreludePrefix :: ByteString
modulePreludePrefix =
  [exon|module Main where
import Preludes
import Prelude.Foo
|]

targetPreludePrefix :: Text
targetPreludePrefix =
  [exon|#{header}
module Main where
import "incipit-base" IncipitBase as Prelude
{-# line 2 "/foo/bar/Foo.hs" #-}
import Preludes
import Prelude.Foo
type Hix_Dummy = Int
{-# line 4 "/foo/bar/Foo.hs" #-}
|]

test_preprocPreludePrefix :: UnitTest
test_preprocPreludePrefix =
  preprocTest modulePreludePrefix targetPreludePrefix
