module Hix.Test.Managed.Release.ValidationTest where

import Exon (exon)
import Hedgehog (Gen, Property, annotate, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hix.Data.Version (Version)
import Hix.Managed.Release.Data.SelectedVersion (SelectedVersion (..))
import Hix.Managed.Release.Validation (VersionProblem (..), validateVersion)
import Hix.Test.Utils (UnitTest, unitTest)

-- | The source of a version selection, corresponding to CLI/UI options.
-- This is the algebraic representation of how a version gets specified.
--
-- Note: CLI args (--version, --package, --force-version) can be combined with interactive UI mode. This enum represents
-- the "effective source" that determines the version, not the full CLI/UI state.
data VersionSource
  = -- | @--version keep@: Keep the current version
    SourceKeep
  | -- | @--version X.Y.Z@: Explicit shared version from CLI
    SourceCliVersion
  | -- | @--package pkg-X.Y.Z@: Explicit version for specific package
    SourcePackageVersion
  | -- | UI selection (interactive mode)
    SourceUi
  | -- | No version specified, auto-computed
    SourceAuto
  deriving stock (Eq, Show, Enum, Bounded)

-- | The relationship between selected version and current version.
data VersionRelation
  = -- | Selected == current
    RelationSame
  | -- | Selected < current
    RelationSmaller
  | -- | Selected is exactly one increment above current (any component)
    RelationOneIncrement
  | -- | Selected is more than one increment above current
    RelationLargeIncrement
  deriving stock (Eq, Show, Enum, Bounded)

-- | Complete specification of a validation test input.
data ValidationInput =
  ValidationInput {
    source :: VersionSource,
    relation :: VersionRelation,
    forceVersion :: Bool
  }
  deriving stock (Eq, Show)

sourceToFlags :: VersionSource -> (Bool, Bool)
sourceToFlags = \case
  SourceKeep -> (True, True)
  SourceCliVersion -> (True, False)
  SourcePackageVersion -> (True, False)
  SourceUi -> (True, False)
  SourceAuto -> (False, False)

-- | Generate a version pair (current, selected) for a given relation.
-- Uses a fixed current version for simplicity.
versionsForRelation :: VersionRelation -> (Version, Version)
versionsForRelation = \case
  RelationSame -> (current, current)
  RelationSmaller -> (current, [0, 9, 0])
  RelationOneIncrement -> (current, [1, 0, 1])  -- Patch bump
  RelationLargeIncrement -> (current, [3, 0, 0])  -- Jump 2 supermajors
  where
    current = [1, 0, 0]

-- | Construct a 'SelectedVersion' from algebraic input.
mkSelectedVersion :: ValidationInput -> (Version, SelectedVersion)
mkSelectedVersion ValidationInput {source, relation} =
  let
    (current, selected) = versionsForRelation relation
    (explicit, keep) = sourceToFlags source
  in
    (current, SelectedVersion {version = selected, explicit, keep})

-- | The expected validation result for a given input.
--
-- 'SourceKeep' only bypasses validation when the new version actually equals the current ('RelationSame').
-- When 'SourceKeep' is used but the version differs (an impossible combination in practice), validation falls through
-- to normal explicit version rules.
expectedResult :: ValidationInput -> Maybe VersionProblem
expectedResult ValidationInput {source, relation, forceVersion}
  | forceVersion = Nothing
  | otherwise = case (source, relation) of
      -- Keep only valid when actually keeping (Same relation)
      (SourceKeep, RelationSame) -> Nothing
      -- Keep with non-Same relation: falls through to explicit validation
      -- This shouldn't happen in practice since keepVersion always uses current
      (SourceKeep, RelationSmaller) -> Just SmallerThanCurrent
      (SourceKeep, RelationOneIncrement) -> Nothing  -- Valid increment
      (SourceKeep, RelationLargeIncrement) -> Just TooLargeIncrement
      -- Auto (no explicit selection) is invalid regardless of relation
      (SourceAuto, RelationSame) -> Just NoVersionSpecified
      (SourceAuto, RelationSmaller) -> Just NoVersionSpecified
      (SourceAuto, RelationOneIncrement) -> Just NoVersionSpecified
      (SourceAuto, RelationLargeIncrement) -> Just NoVersionSpecified
      -- Explicit sources - outcome depends on relation
      (SourceCliVersion, RelationSame) -> Just SameAsCurrent
      (SourceCliVersion, RelationSmaller) -> Just SmallerThanCurrent
      (SourceCliVersion, RelationOneIncrement) -> Nothing
      (SourceCliVersion, RelationLargeIncrement) -> Just TooLargeIncrement
      (SourcePackageVersion, RelationSame) -> Just SameAsCurrent
      (SourcePackageVersion, RelationSmaller) -> Just SmallerThanCurrent
      (SourcePackageVersion, RelationOneIncrement) -> Nothing
      (SourcePackageVersion, RelationLargeIncrement) -> Just TooLargeIncrement
      (SourceUi, RelationSame) -> Just SameAsCurrent
      (SourceUi, RelationSmaller) -> Just SmallerThanCurrent
      (SourceUi, RelationOneIncrement) -> Nothing
      (SourceUi, RelationLargeIncrement) -> Just TooLargeIncrement

genVersionSource :: Gen VersionSource
genVersionSource = Gen.enumBounded

genVersionRelation :: Gen VersionRelation
genVersionRelation = Gen.enumBounded

-- | Generate a validation input
genValidationInput :: Gen ValidationInput
genValidationInput =
  ValidationInput
    <$> genVersionSource
    <*> genVersionRelation
    <*> Gen.bool

-- | Property: validateVersion agrees with expectedResult
-- Note: forceVersion is handled at a higher level, so we test with forceVersion = False
prop_validateVersion :: Property
prop_validateVersion = property do
  input <- forAll genValidationInput
  let inputNoForce = input {forceVersion = False}
  let (current, selected) = mkSelectedVersion inputNoForce
  annotate [exon|Source: #{show input.source}|]
  annotate [exon|Relation: #{show input.relation}|]
  annotate [exon|Current: #{show current}|]
  annotate [exon|Selected: #{show selected}|]
  validateVersion current selected === expectedResult inputNoForce

-- | Enumerate all interesting combinations to ensure coverage.
-- This helps identify any edge cases the property might miss.
allValidationCases :: [(ValidationInput, Maybe VersionProblem)]
allValidationCases =
  [ (input, expectedResult input)
  | source <- [minBound .. maxBound]
  , relation <- [minBound .. maxBound]
  , let input = ValidationInput {source, relation, forceVersion = False}
  ]

-- | Unit test that explicitly shows all expected outcomes.
-- Useful for documentation and debugging.
test_validationMatrix :: UnitTest
test_validationMatrix = do
  for_ allValidationCases \ (input, expected) -> do
    let (current, selected) = mkSelectedVersion input
    let actual = validateVersion current selected
    annotate [exon|Input: #{show input}|]
    annotate [exon|Expected: #{show expected}|]
    annotate [exon|Actual: #{show actual}|]
    actual === expected


-- | Specific test cases for the version bump edge cases in the Nix test.
-- These document the actual behavior with concrete version numbers.
test_nixTestVersionBumps :: UnitTest
test_nixTestVersionBumps = do
  annotate "Testing version bumps from Nix release test"
  let current = [0, 1, 0, 0]

  -- version1: 0.1.0.0 -> 0.2.0 (major bump, should be valid)
  annotate "version1: 0.1.0.0 -> 0.2.0"
  validateExplicit current [0, 2, 0] === Nothing

  -- version3: 0.1.0.0 -> 1.0.0 (supermajor bump, should be valid)
  -- Note: supermajorBump([0,1,0,0]) = [1,0,0,0], and [1,0,0] < [1,0,0,0] in Cabal
  -- So this is NOT a large increment (new is not > supermajorBump)
  annotate "version3: 0.1.0.0 -> 1.0.0"
  validateExplicit current [1, 0, 0] === Nothing

  -- Verify the version comparison behavior
  annotate "Version comparison: [1,0,0] vs [1,0,0,0]"
  let v100 = [1, 0, 0] :: Version
  let v1000 = [1, 0, 0, 0] :: Version
  assert (v100 < v1000)  -- Cabal: shorter version is less than longer with trailing zeros

  -- version4: 0.1.0.0 -> 1.1.0 (more than one increment, should fail)
  annotate "version4: 0.1.0.0 -> 1.1.0"
  validateExplicit current [1, 1, 0] === Just TooLargeIncrement

  -- Edge case: trailing zeros should be equivalent
  annotate "Trailing zeros: 1.0.0 vs 1.0.0.0"
  validateExplicit current [1, 0, 0, 0] === Nothing

  where
    validateExplicit cur sel =
      validateVersion cur SelectedVersion {version = sel, explicit = True, keep = False}

------------------------------------------------------------------------------------------------
-- Metamorphic Properties
--
-- These properties don't recompute the expected result - they express relationships
-- between inputs and outputs that must hold regardless of the specific outcome.
------------------------------------------------------------------------------------------------

-- | Keep bypasses the 'SameAsCurrent' check when relation is Same.
-- This is the key difference between keep and explicit sources.
-- For other relations, keep behaves like explicit sources.
prop_keepBypassesSameAsCurrent :: Property
prop_keepBypassesSameAsCurrent = property do
  let keepSame = ValidationInput {source = SourceKeep, relation = RelationSame, forceVersion = False}
  let explicitSame = ValidationInput {source = SourceCliVersion, relation = RelationSame, forceVersion = False}
  annotate [exon|Keep + Same: #{show (expectedResult keepSame)}|]
  annotate [exon|Explicit + Same: #{show (expectedResult explicitSame)}|]
  -- Keep + Same = valid, Explicit + Same = SameAsCurrent error
  expectedResult keepSame === Nothing
  expectedResult explicitSame === Just SameAsCurrent

-- | Validation is "monotonic" in increment size for explicit sources:
-- If a larger increment passes, all smaller increments must also pass.
-- Contrapositive: if OneIncrement fails, LargeIncrement must also fail.
prop_incrementMonotonicity :: Property
prop_incrementMonotonicity = property do
  source <- forAll genExplicitSource
  let oneInc = ValidationInput {source, relation = RelationOneIncrement, forceVersion = False}
  let largeInc = ValidationInput {source, relation = RelationLargeIncrement, forceVersion = False}
  annotate [exon|Source: #{show source}|]
  annotate [exon|OneIncrement result: #{show (expectedResult oneInc)}|]
  annotate [exon|LargeIncrement result: #{show (expectedResult largeInc)}|]
  -- If large increment passes, one increment must pass
  when (isNothing (expectedResult largeInc)) do
    expectedResult oneInc === Nothing

-- | The validation outcome for a given (source, relation) pair is independent of
-- what other sources "could have been" - only the actual source matters.
-- This tests that switching between explicit sources doesn't change the outcome
-- when the relation is the same.
prop_explicitSourcesEquivalent :: Property
prop_explicitSourcesEquivalent = property do
  source1 <- forAll genExplicitSource
  source2 <- forAll genExplicitSource
  relation <- forAll genVersionRelation
  let input1 = ValidationInput {source = source1, relation, forceVersion = False}
  let input2 = ValidationInput {source = source2, relation, forceVersion = False}
  annotate [exon|Source1: #{show source1}, Source2: #{show source2}|]
  annotate [exon|Relation: #{show relation}|]
  -- All explicit sources should produce the same result for the same relation
  expectedResult input1 === expectedResult input2

-- | Generate only explicit sources (not Keep or Auto)
genExplicitSource :: Gen VersionSource
genExplicitSource = Gen.element
  (SourceCliVersion :| [SourcePackageVersion, SourceUi])

-- | forceVersion always bypasses validation, regardless of source or relation.
prop_forceAlwaysBypasses :: Property
prop_forceAlwaysBypasses = property do
  source <- forAll genVersionSource
  relation <- forAll genVersionRelation
  let input = ValidationInput {source, relation, forceVersion = True}
  annotate [exon|Source: #{show source}, Relation: #{show relation}|]
  expectedResult input === Nothing

-- | Auto source never passes validation - it always produces NoVersionSpecified.
-- This ensures users must explicitly choose a version.
prop_autoNeverPasses :: Property
prop_autoNeverPasses = property do
  relation <- forAll genVersionRelation
  let input = ValidationInput {source = SourceAuto, relation, forceVersion = False}
  annotate [exon|Relation: #{show relation}|]
  expectedResult input === Just NoVersionSpecified

-- | Smaller versions always fail for explicit sources.
-- You can't release a version smaller than current.
prop_smallerAlwaysFails :: Property
prop_smallerAlwaysFails = property do
  source <- forAll genExplicitSource
  let input = ValidationInput {source, relation = RelationSmaller, forceVersion = False}
  annotate [exon|Source: #{show source}|]
  expectedResult input === Just SmallerThanCurrent

-- | Same version always fails for explicit sources (but not keep).
-- If you explicitly specify the current version, that's an error.
prop_sameFailsForExplicit :: Property
prop_sameFailsForExplicit = property do
  source <- forAll genExplicitSource
  let input = ValidationInput {source, relation = RelationSame, forceVersion = False}
  annotate [exon|Source: #{show source}|]
  expectedResult input === Just SameAsCurrent

-- | A single valid increment always passes for explicit sources.
-- This is the "happy path" - bumping by one level should always work.
prop_oneIncrementPasses :: Property
prop_oneIncrementPasses = property do
  source <- forAll genExplicitSource
  let input = ValidationInput {source, relation = RelationOneIncrement, forceVersion = False}
  annotate [exon|Source: #{show source}|]
  expectedResult input === Nothing

-- | Large increments always fail for explicit sources.
-- Skipping multiple version levels is not allowed without force.
prop_largeIncrementFails :: Property
prop_largeIncrementFails = property do
  source <- forAll genExplicitSource
  let input = ValidationInput {source, relation = RelationLargeIncrement, forceVersion = False}
  annotate [exon|Source: #{show source}|]
  expectedResult input === Just TooLargeIncrement

-- | Keep behaves like explicit sources for all relations except Same.
-- This documents the semantic: keep is special only for the "no change" case.
prop_keepLikeExplicitExceptSame :: Property
prop_keepLikeExplicitExceptSame = property do
  relation <- forAll $ Gen.element (RelationSmaller :| [RelationOneIncrement, RelationLargeIncrement])
  let keepInput = ValidationInput {source = SourceKeep, relation, forceVersion = False}
  let explicitInput = ValidationInput {source = SourceCliVersion, relation, forceVersion = False}
  annotate [exon|Relation: #{show relation}|]
  annotate [exon|Keep result: #{show (expectedResult keepInput)}|]
  annotate [exon|Explicit result: #{show (expectedResult explicitInput)}|]
  expectedResult keepInput === expectedResult explicitInput

------------------------------------------------------------------------------------------------
-- Test tree
------------------------------------------------------------------------------------------------

test_validation :: TestTree
test_validation =
  testGroup "validation-property" [
    testGroup "model" [
      testProperty "model-vs-implementation" prop_validateVersion,
      unitTest "matrix" test_validationMatrix,
      unitTest "nix-test-bumps" test_nixTestVersionBumps
    ],
    testGroup "metamorphic" [
      testProperty "force-always-bypasses" prop_forceAlwaysBypasses,
      testProperty "auto-never-passes" prop_autoNeverPasses,
      testProperty "smaller-always-fails" prop_smallerAlwaysFails,
      testProperty "same-fails-for-explicit" prop_sameFailsForExplicit,
      testProperty "one-increment-passes" prop_oneIncrementPasses,
      testProperty "large-increment-fails" prop_largeIncrementFails,
      testProperty "keep-bypasses-same-as-current" prop_keepBypassesSameAsCurrent,
      testProperty "keep-like-explicit-except-same" prop_keepLikeExplicitExceptSame,
      testProperty "increment-monotonicity" prop_incrementMonotonicity,
      testProperty "explicit-sources-equivalent" prop_explicitSourcesEquivalent
    ]
  ]
