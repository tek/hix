module Hix.Test.Managed.TargetSpecTest where

import Hedgehog (Property, property, test, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hix.Data.PackageId (PackageId (..))
import Hix.Managed.Release.Data.TargetSpec (TargetSpec (..))

-- | correctly handles package names ending in digits
prop_packageNameEndingInDigit :: Property
prop_packageNameEndingInDigit = property $ test do
  -- "local1" should be parsed as just a package name, not as "local-1"
  "local1" === TargetName "local1"
  -- "foo2bar" should also be just a package name
  "foo2bar" === TargetName "foo2bar"

-- | correctly parses explicit versions with hyphen-digit pattern
prop_explicitVersion :: Property
prop_explicitVersion = property $ test do
  -- "foo-1.2.3" should be parsed as package "foo" with version "1.2.3"
  "foo-1.2.3" === TargetExplicit PackageId {name = "foo", version = [1, 2, 3]}
  -- "local1-2.0.0" should be package "local1" with version "2.0.0"
  "local1-2.0.0" === TargetExplicit PackageId {name = "local1", version = [2, 0, 0]}

-- | handles edge cases
prop_edgeCases :: Property
prop_edgeCases = property $ test do
  -- Package name with hyphen but no version (hyphen not followed by digit)
  "foo-bar" === TargetName "foo-bar"
  -- Single digit version
  "foo-1" === TargetExplicit PackageId {name = "foo", version = [1]}
  -- Complex version
  "my-pkg-1.2.3.4" === TargetExplicit PackageId {name = "my-pkg", version = [1, 2, 3, 4]}

test_targetSpec :: TestTree
test_targetSpec =
  testGroup "TargetSpec parsing" [
    testProperty "package name ending in digit" prop_packageNameEndingInDigit,
    testProperty "explicit version" prop_explicitVersion,
    testProperty "edge cases" prop_edgeCases
  ]
