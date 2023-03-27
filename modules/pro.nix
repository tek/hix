{ config, lib, ... }:
with lib;
{
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
      "MultiWayIf"
      "OverloadedLabels"
      "OverloadedLists"
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
    ] ++ optionals (versionAtLeast config.envs.dev.ghc.version "9.2") ["OverloadedRecordDot" "NoFieldSelectors"];

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

  ghci.ghcOptions = ["-Werror"];

}
