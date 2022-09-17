{ config, lib, ... }:
with lib;
let

  extensions = [
    "AllowAmbiguousTypes"
    "ApplicativeDo"
    "BangPatterns"
    "BinaryLiterals"
    "BlockArguments"
    "ConstraintKinds"
    "DataKinds"
    "DefaultSignatures"
    "DeriveAnyClass"
    "DeriveDataTypeable"
    "DeriveFoldable"
    "DeriveFunctor"
    "DeriveGeneric"
    "DeriveLift"
    "DeriveTraversable"
    "DerivingStrategies"
    "DerivingVia"
    "DisambiguateRecordFields"
    "DoAndIfThenElse"
    "DuplicateRecordFields"
    "EmptyCase"
    "EmptyDataDecls"
    "ExistentialQuantification"
    "FlexibleContexts"
    "FlexibleInstances"
    "FunctionalDependencies"
    "GADTs"
    "GeneralizedNewtypeDeriving"
    "InstanceSigs"
    "KindSignatures"
    "LambdaCase"
    "LiberalTypeSynonyms"
    "MultiParamTypeClasses"
    "MultiWayIf"
    "NamedFieldPuns"
    "OverloadedLabels"
    "OverloadedLists"
    "OverloadedStrings"
    "PackageImports"
    "PartialTypeSignatures"
    "PatternGuards"
    "PatternSynonyms"
    "PolyKinds"
    "QuantifiedConstraints"
    "QuasiQuotes"
    "RankNTypes"
    "RecordWildCards"
    "RecursiveDo"
    "RoleAnnotations"
    "ScopedTypeVariables"
    "StandaloneDeriving"
    "TemplateHaskell"
    "TupleSections"
    "TypeApplications"
    "TypeFamilies"
    "TypeFamilyDependencies"
    "TypeOperators"
    "TypeSynonymInstances"
    "UndecidableInstances"
    "UnicodeSyntax"
    "ViewPatterns"
  ];
in {

  ghci = {

    inherit extensions;

  };

}
