module Hix.Test.PreprocTest where

import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Text as Text
import Distribution.PackageDescription (
  CondTree (condTreeData),
  GenericPackageDescription (condLibrary),
  Library (libBuildInfo),
  )
import Exon (exon)
import Hedgehog (TestT, evalMaybe, (===))
import Hix.Preproc (preprocessModule)
import Path (absfile)

import Hix.Test.CabalFile (testPackage, testPackageNoPrelude)

pragmas :: Text
pragmas =
  [exon|{-# options_ghc -Wall -Wunused-imports #-}
{-# language AllowAmbiguousTypes, NoApplicativeDo #-}|]

preprocTestNoPrelude ::
  HasCallStack =>
  ByteString ->
  Text ->
  TestT IO ()
preprocTestNoPrelude module_ target =
  withFrozenCallStack do
    pkg <- liftIO testPackageNoPrelude
    info <- evalMaybe (pkg.condLibrary <&> \ l -> l.condTreeData.libBuildInfo)
    let result = toLazyByteString (preprocessModule [absfile|/foo/bar/Foo.hs|] info "Hix_Dummy" module_)
    Text.lines target === Text.lines (decodeUtf8 result)

preprocTest ::
  HasCallStack =>
  ByteString ->
  Text ->
  TestT IO ()
preprocTest module_ target =
  withFrozenCallStack do
    pkg <- liftIO testPackage
    info <- evalMaybe (pkg.condLibrary <&> \ l -> l.condTreeData.libBuildInfo)
    let result = toLazyByteString (preprocessModule [absfile|/foo/bar/Foo.hs|] info "Hix_Dummy" module_)
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
  [exon|#{pragmas}
{-# language PackageImports, NoImplicitPrelude #-}
{-# line 1 "/foo/bar/Foo.hs" #-}
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

test_preprocInsertPrelude :: TestT IO ()
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
import Data.Maybe

f :: Int
f = 1
|]

targetReplace :: Text
targetReplace =
  [exon|#{pragmas}
{-# language PackageImports, NoImplicitPrelude #-}
{-# line 1 "/foo/bar/Foo.hs" #-}
{-# language OverloadedLabels #-}

module Foo (Hix_Dummy,
  moo,

  noo,
  ) where

import Data.Char
import "incipit-base" IncipitBase (sum)
import Data.List
import "incipit-base" IncipitBase hiding (Maybe)
import Data.Maybe

type Hix_Dummy = Int
{-# line 15 "/foo/bar/Foo.hs" #-}
f :: Int
f = 1
|]

test_preprocReplacePrelude :: TestT IO ()
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
  [exon|#{pragmas}
{-# language PackageImports, NoImplicitPrelude #-}
{-# line 1 "/foo/bar/Foo.hs" #-}
module Foo where
import "incipit-base" IncipitBase as Prelude
{-# line 2 "/foo/bar/Foo.hs" #-}

type Hix_Dummy = Int
{-# line 3 "/foo/bar/Foo.hs" #-}
f :: Int
f = 1
|]

test_preprocSingleLineModule :: TestT IO ()
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
  [exon|#{pragmas}
{-# language PackageImports, NoImplicitPrelude #-}
{-# line 1 "/foo/bar/Foo.hs" #-}
module Foo (module Foo) where
import "incipit-base" IncipitBase as Prelude
{-# line 2 "/foo/bar/Foo.hs" #-}

type Hix_Dummy = Int
{-# line 3 "/foo/bar/Foo.hs" #-}
f :: Int
f = 1
|]

test_preprocSelfExport :: TestT IO ()
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
  [exon|#{pragmas}
{-# language PackageImports, NoImplicitPrelude #-}
{-# line 1 "/foo/bar/Foo.hs" #-}
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

test_preprocSelfExport2 :: TestT IO ()
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

test_preprocNoPrelude :: TestT IO ()
test_preprocNoPrelude =
  preprocTestNoPrelude moduleNoPrelude targetNoPrelude
