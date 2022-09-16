{
  lib,
  config,
  ...
}:
with builtins;
with lib;
let

  pkgs = config.internal.basicPkgs;

  newerThan810 = match "ghc(81|9).*" config.devGhc.compiler != null;

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

  basicGhciArgs = [
    "-Werror"
    "-Wall"
    "-Wredundant-constraints"
    "-Wunused-type-patterns"
    "-Widentities"
  ];

  libDir = pkg:
  "$PWD/" + (if pkg == "." then "lib" else "${pkg}/lib");

  srcDir = pkg:
  "$PWD/" + (if pkg == "." then "src" else "${pkg}/src");

  colonSeparated =
    concatStringsSep ":";

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

  preludeFix = config.ghci.preludePackage != null || config.ghci.preludeModule != null;

  preludeSearchCode = pkg: module: ''
  module Prelude (module ${module}) where
  import ${pkg} ${module}
  '';

  preludeSearchFile =
    let
      module =
        if config.ghci.preludeModule == null
        then "Prelude"
        else config.ghci.preludeModule;
    in
      if config.ghci.preludePackage == null
      then preludeSearchCode "" module
      else preludeSearchCode ''"${config.ghci.preludePackage}"'' module;

  preludeSearch =
    let
      preludeDir = pkgs.writeTextFile {
        name = "prelude-reexport";
        text = preludeSearchFile;
        destination = "/Prelude.hs";
      };
    in
      "${preludeDir}";

  ghciScript = cwd: script:
  let
    scriptText = if isAttrs script then script.script else script;
  in
    cwdScript cwd + optionalString preludeFix preludeScript + scriptText;

  command = {
    packages,
    script,
    search,
    cwd ? null,
  }:
  let
    searchP = searchPaths (map srcDir (attrValues packages) ++ map libDir (attrValues packages) ++ search ++ optional preludeFix preludeSearch);
    script' = ghciScript cwd script;
    scriptFile = pkgs.writeText "ghci-script" script';
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
        The command line arguments passed to GHCi.
        Setting this option appends to the defaults, so in order to replace them, use 'mkForce'.
      '';
    };

    scripts = mkOption {
      description = ''
        A set of Cabal scripts, to be referenced by entries in <literal>ghcid.commands</literal>.
      '';
      type = attrsOf (functionTo lines);
      example = literalExpression ''
      {
        :set args --port 8080
        :load Main
        import Main
      }
    '';
    };

    runners = mkOption {
      description = ''
        A function returning Haskell code that should be executed by GHCi when running a test.
        The argument is the test function name, as passed in via command line arguments.
      '';
      type = attrsOf (functionTo str);
    };

    options_ghc = mkOption {
      description = "Global GHC options inserted into each file processed by GHCi.";
      type = str;
      default = "";
    };

    extensions = mkOption {
      description = ''
        Global GHC extensions inserted into each file processed by GHCi.
        Since <literal>ghcid</literal> cannot read Cabal files, these have to be set manually.
      '';
      type = listOf str;
    };

    preprocessor = mkOption {
      description = ''
        The preprocessor script used to insert <literal>ghci.options_ghc</literal> and
        <literal>ghci.extensions</literal> into source files.
        This is generated by Hix, but may be overridden.
      '';
      type = path;
    };

    preprocessorExtraCode = mkOption {
      description = ''
        Extra Haskell code to be inserted into source files by the preprocessor.
      '';
      type = lines;
      default = "";
    };

    preludePackage = mkOption {
      description = ''
        The Cabal package in which the <literal>Prelude</literal> is defined.
        This will be used to disambiguate the import of a custom prelude <literal>Prelude</literal>.
        If this option and <literal>ghci.preludeModule</literal> are both <literal>null</literal>, no action will be
        taken.
        If any of the two are set, GHCi will be started with <literal>-XNoImplicitPrelude</literal>
        and a temporary search path containing a reexport of the <literal>Prelude</literal> will be set.
      '';
      type = nullOr str;
      default = null;
      example = "relude";
    };

    preludeModule = mkOption {
      description = ''
      The module name of the custom <literal>Prelude</literal>.
      See <literal>preludePackage</literal> for details.
      If <literal>preludePackage</literal> is not set, it is assumed that this module is part of the current project.
      '';
      type = nullOr str;
      default = null;
      example = "Relude";
    };

    command = mkOption {
      description = "Internal API function for creating GHCi commands.";
      type = functionTo unspecified;
    };

  };

  config.ghci = {
    inherit extensions;

    preprocessor = mkDefault (import ../lib/preprocessor.nix {
      inherit pkgs;
      inherit (config.ghci) extensions;
      extraCode = config.ghci.preprocessorExtraCode;
    });

    args =
      basicGhciArgs
      ++
      optional newerThan810 "-Wunused-packages"
      ++
      optional preludeFix "-XNoImplicitPrelude -XPackageImports"
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
