{util, ...}:
{
  ghcVersions = ["ghc92" "ghc94" "ghc96"];

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
    ] ++
    (
      if util.minGhcs "9.2"
      then ["OverloadedRecordDot" "NoFieldSelectors"]
      else [
        "BangPatterns"
        "BinaryLiterals"
        "ConstraintKinds"
        "DeriveDataTypeable"
        "DeriveFoldable"
        "DeriveFunctor"
        "DeriveGeneric"
        "DeriveLift"
        "DeriveTraversable"
        "DoAndIfThenElse"
        "EmptyCase"
        "EmptyDataDecls"
        "ExistentialQuantification"
        "FlexibleContexts"
        "FlexibleInstances"
        "GeneralizedNewtypeDeriving"
        "InstanceSigs"
        "KindSignatures"
        "MultiParamTypeClasses"
        "NamedFieldPuns"
        "PatternGuards"
        "PolyKinds"
        "RankNTypes"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TupleSections"
        "TypeApplications"
        "TypeOperators"
        "TypeSynonymInstances"
      ]
    );

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

  hackage.setChangelogVersion = true;

}
