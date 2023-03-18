{ global, util }:
{ name, lib, config, ... }:
with lib;
let

  pkgConfig = config;
  pkgName = name;

  cabalOptionsModule = import ./cabal-options.nix { inherit util; };
  cabalComponentModule = import ./cabal-component.nix;

  libModule = {...}: {

    options = with types; {
    };

  };

  anyEnabled = set: any (a: a.enable) (attrValues set);

  exeModule = sort: default: {name ? pkgName, config, ...}: {

    options = with types; {

      main = mkOption {
        type = str;
        description = "The file name of the main module.";
        default = "Main.hs";
      };

    };

    config = let
      hasComponents =
        pkgConfig.library.enable || anyEnabled pkgConfig.executables ||
        pkgConfig.test.enable || anyEnabled pkgConfig.tests ||
        pkgConfig.benchmark.enable || anyEnabled pkgConfig.benchmarks;
    in optionalAttrs default {
      enable = mkDefault (!hasComponents);
    } // {
      dependencies = optional (config.dependOnLibrary && pkgConfig.library.enable) pkgName;

      ghc-options = mkIf (sort != "benchmark") config.ghc-options-exe;
    };

  };

  withoutVerbatim = set: removeAttrs set ["cabal"];

  component = main: src: sort: suffix: single:
    types.submoduleWith {
      modules = [
        cabalOptionsModule
        (cabalComponentModule { inherit pkgName src sort suffix single; })
        (withoutVerbatim global.cabal)
        (withoutVerbatim global.internal.cabal-extra)
        (withoutVerbatim pkgConfig.cabal)
        main
    ];
    };

  libSubmodule = component libModule "lib" "library" null true;

  exeSubmodule = default: component (exeModule "executable" default) "app" "executable" "exeSuffix";

  testSubmodule = component (exeModule "test" false) "test" "test suite" "testSuffix";

  benchSubmodule = component (exeModule "benchmark" false) "benchmark" "benchmark" "benchSuffix";

in {

  options = with types; {

    name = mkOption {
      description = "The name of the package, defaulting to the attribute name in the config.";
      type = str;
      default = name;
    };

    src = mkOption {
      description = "The root directory of the package.";
      type = path;
      example = literalExpression "./packages/api";
    };

    library = mkOption {
      description = ''
      The library for this package.
      '';
      type = libSubmodule;
      default = {};
    };

    executable = mkOption {
      description = ''
      The single executable for this package.
      To define multiple executables, use {option}`executables`.
      '';
      type = exeSubmodule true true;
      default = {};
    };

    executables = mkOption {
      description = ''
      Executables for this package.
      If {option}`executable` is defined, it will be added.
      '';
      type = attrsOf (exeSubmodule false false);
      default = {};
    };

    test = mkOption {
      description = ''
      The single test suite for this package.
      To define multiple test suites, use {option}`tests`.
      '';
      type = testSubmodule true;
      default = {};
    };

    tests = mkOption {
      description = ''
      Test suites for this package.
      If {option}`test` is defined, it will be added.
      '';
      type = attrsOf (testSubmodule false);
      default = {};
    };

    benchmark = mkOption {
      description = ''
      The single benchmark for this package.
      To define multiple benchmarks, use {option}`benchmarks`.
      '';
      type = benchSubmodule true;
      default = {};
    };

    benchmarks = mkOption {
      description = ''
      Benchmarks for this package.
      If {option}`benchmark` is defined, it will be added.
      '';
      type = attrsOf (benchSubmodule false);
      default = {};
    };

    rootModule = mkOption {
      description = ''
      A convenience option that is used to generate a Hackage link.
      It should denote the module that represents the most high-level API of the package, if applicable.
      The default is to replace dashes in the name with dots.
      '';
      type = str;
    };

    hackageLink = mkOption {
      description = ''
      A convenience option containing the URL to the Hackage page using the package name.
      '';
      type = str;
    };

    hackageRootLink = mkOption {
      description = ''
      A convenience option containing the URL to the root module's documentation on Hackage using the package name and
      {option}`rootModule`.
      '';
      type = str;
    };

    description = mkOption {
      description = ''
      The Cabal description of this packages.
      The default is a link to the {option}`rootModule` on Hackage, using the option {option}`hackageRootLink`.
      May be `null` to omit it from the config.
      '';
      type = nullOr str;
      default = null;
    };

    cabal = mkOption {
      type = unspecified;
      description = ''
      Cabal options that are applied to all components.
      **Note**: In order to enable cascading of these options, the definitions are not evaluated in-place, but when
      evaluating components. Therefore, referring to these values with e.g.
      <literal>config.packages.name.cabal.version</literal> does not work as expected if the value uses an option
      property like <literal>mkIf</literal> or <literal>mkOverride</literal>.
      You can use {option}`cabal-config` for this purpose, though.
      '';
      default = {};
    };

    cabal-config = mkOption {
      type = submoduleWith { modules =
        [
          (withoutVerbatim global.internal.cabal-extra)
          (withoutVerbatim global.cabal)
          config.cabal
          cabalOptionsModule
        ];
      };
      readOnly = true;
      description = ''
      Evaluated version of {option}`cabal`, for referencing in other config values.
      May not be set by the user.
      '';
      default = {};
    };

  };

  config = {

    rootModule = mkDefault (concatMapStringsSep "." util.toTitle (splitString "-" config.name));

    hackageLink = mkDefault "https://hackage.haskell.org/package/${config.name}";

    hackageRootLink = mkDefault "${config.hackageLink}/docs/${config.rootModule}.html";

    description = mkDefault "See ${config.hackageRootLink}";

  };

}
