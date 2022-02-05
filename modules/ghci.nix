{
  lib,
  config,
  ...
}:
with lib;
let

  newerThan810 = builtins.match "ghc(81|9).*" config.devGhc.compiler != null;

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
    "OverloadedStrings"
    "OverloadedLists"
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

  basicGhciArgs = [
    "-Werror"
    "-Wall"
    "-Wredundant-constraints"
    "-Wunused-type-patterns"
    "-Widentities"
  ];

  libDir = pkg:
  "$PWD/" + (if pkg == "." then "lib" else "${pkg}/lib");

  colonSeparated =
    builtins.concatStringsSep ":";

  searchPaths = paths:
    "-i${colonSeparated paths}";

  inherit (config.ghci) options_ghc;

  preludeScript =
    ''
      :load Prelude
      import Prelude
      :set -XImplicitPrelude
    '';

  cwdScript = cwd:
  optionalString (cwd != null) ''
    :cd ${cwd}
  '';

  builtinTestScripts = {
    hedgehog-property = module: ''
      :load ${module}
      import ${module}
      import Hedgehog (check)
    '';

    hedgehog-unit = module: ''
      :load ${module}
      import ${module}
      import Hedgehog (check, property, test, withTests)
    '';

    tasty-tree = module: ''
      :load ${module}
      import ${module}
      import Test.Tasty (defaultMain)
    '';

    generic = module: ''
      :load ${module}
      import ${module}
    '';
  };

  builtinTestRunners = {
    hedgehog-property = name: "check ${name}";
    hedgehog-unit = name: "(check . withTests 1 . property . test) ${name}";
    tasty-tree = name: "defaultMain ${name}";
    generic = id;
  };

  ghciScript = cwd: script:
  let
    scriptText = if builtins.isAttrs script then script.script else script;
  in
    cwdScript cwd + optionalString config.ghci.prelude preludeScript + scriptText;

  command = {
    packages,
    script,
    search,
    cwd ? null,
  }:
  let
    searchP = searchPaths ((map libDir (builtins.attrValues packages)) ++ search);
    script' = ghciScript cwd script;
    scriptFile = config.internal.basicPkgs.writeText "ghci-script" script';
  in {
    inherit searchP scriptFile;
    script = script';
    cmdline = "ghci ${toString config.ghci.args} ${searchP} -ghci-script ${scriptFile}";
  };

in {

  options.ghci = with types; {

    args = mkOption {
      type = listOf str;
      description = ''
        The command line arguments passed to ghci.
        Setting this option appends to the defaults, so in order to replace them, use 'mkForce'.
      '';
    };

    scripts = mkOption {
      type = attrsOf (functionTo lines);
    };

    runners = mkOption {
      type = attrsOf (functionTo str);
    };

    options_ghc = mkOption {
      type = str;
      default = "";
    };

    extensions = mkOption {
      type = listOf str;
    };

    preprocessor = mkOption {
      type = path;
    };

    preprocessorExtraCode = mkOption {
      type = lines;
      default = "";
    };

    prelude = mkOption {
      type = bool;
      default = true;
    };

    command = mkOption {
      type = functionTo unspecified;
    };

  };

  config.ghci = {
    inherit extensions;

    preprocessor = mkDefault (import ../lib/preprocessor.nix {
      pkgs = config.internal.basicPkgs;
      inherit (config.ghci) extensions;
      extraCode = config.ghci.preprocessorExtraCode;
    });

    args =
      basicGhciArgs
      ++
      optional newerThan810 "-Wunused-packages"
      ++
      optional config.ghci.prelude "-XNoImplicitPrelude"
      ++
      ["-F" "-pgmF" (toString config.ghci.preprocessor)]
      ++
      optionals (config.ghci.options_ghc != "") ["-optF" config.ghci.options_ghc]
      ;

    scripts = builtinTestScripts;
    runners = builtinTestRunners;

    command = mkDefault command;
  };
}
