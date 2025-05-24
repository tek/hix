{util, lib, ...}:
{
  ghcVersions = util.lib.mkOverride 500 ["ghc92" "ghc94" "ghc96"];

  internal.cabal-extra = {

    default-extensions = [
      "AllowAmbiguousTypes"
      "ApplicativeDo"
      "BlockArguments"
      "DataKinds"
      "DefaultSignatures"
      "DeriveAnyClass"
      "DerivingStrategies"
      "DerivingVia"
      "DisambiguateRecordFields"
      "DuplicateRecordFields"
      "FunctionalDependencies"
      "GADTs"
      "LambdaCase"
      "LiberalTypeSynonyms"
      "MonadComprehensions"
      "MultiWayIf"
      "NoFieldSelectors"
      "OverloadedLabels"
      "OverloadedLists"
      "OverloadedRecordDot"
      "OverloadedStrings"
      "PackageImports"
      "PartialTypeSignatures"
      "PatternSynonyms"
      "QuantifiedConstraints"
      "QuasiQuotes"
      "RecordWildCards"
      "RecursiveDo"
      "RoleAnnotations"
      "TemplateHaskell"
      "TypeFamilies"
      "TypeFamilyDependencies"
      "UndecidableInstances"
      "UnicodeSyntax"
      "ViewPatterns"
    ];

    ghc-options = [
      "-Wall"
      "-Widentities"
      "-Wincomplete-uni-patterns"
      "-Wmissing-deriving-strategies"
      "-Wredundant-constraints"
      "-Wunused-type-patterns"
      "-Wunused-packages"
    ];

  };

  ghci.ghcOptions = ["-Werror"] ++ lib.optional (util.minGhcDev "9.8") "-fno-show-error-context";

  hackage.setChangelogVersion = true;

}
