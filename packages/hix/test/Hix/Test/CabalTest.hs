module Hix.Test.CabalTest where

import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString as ByteString
import Distribution.Package (Dependency)
import qualified Distribution.PackageDescription
import Hedgehog ((===))
import Language.Haskell.Extension (
  Extension (DisableExtension, EnableExtension),
  KnownExtension (AllowAmbiguousTypes, ApplicativeDo),
  )
import Path (relfile, toFilePath, (</>))
import Path.IO (withSystemTempDir)

import Hix.Cabal (parseCabal)
import qualified Hix.Data.Dep as Dep
import Hix.Test.CabalFile (testCabal)
import Hix.Test.Utils (UnitTest)

target :: [Extension]
target =
  [EnableExtension AllowAmbiguousTypes, DisableExtension ApplicativeDo]

deps :: [Dependency]
deps =
  Dep.toCabal <$> ["Cabal", "base >=4.12 && <5", "semigroups >=1"]

test_cabal :: UnitTest
test_cabal = do
  Right pkg <- liftIO $ withSystemTempDir "hix-unit" \ tmp -> do
    let cabalFile = tmp </> [relfile|test.cabal|]
    ByteString.writeFile (toFilePath cabalFile) testCabal
    runExceptT (parseCabal cabalFile)
  let lib = fold pkg.condLibrary
  deps === lib.condTreeData.libBuildInfo.targetBuildDepends
  deps === lib.condTreeConstraints
  target === lib.condTreeData.libBuildInfo.defaultExtensions
