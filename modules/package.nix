{ global, util }:
{ name, lib, config, ... }:
with lib;
let

  pkgConfig = config;
  pkgName = name;

  cabalOptionsModule = import ./cabal-options.nix { inherit global util; };
  cabalComponentModule = import ./cabal-component.nix { inherit global util; };

  anyEnabled = set: any (a: a.enable) (attrValues set);

  # excludes `pkgConfig.executable` because it's used to determine whether the default executable should be enabled.
  hasComponents =
    pkgConfig.library.enable || anyEnabled pkgConfig.libraries ||
    anyEnabled pkgConfig.executables ||
    pkgConfig.test.enable || anyEnabled pkgConfig.tests ||
    pkgConfig.benchmark.enable || anyEnabled pkgConfig.benchmarks;

  libModule = {name ? pkgName, config, ...}: {

    options = with types; {

      reexported-modules = mkOption {
        description = mdDoc "Modules from dependencies that this library exposes for downstream projects to import.";
        type = listOf str;
        example = literalExpression ''["Control.Concurrent.STM" "Data.Text"]'';
        default = [];
      };

      public = mkOption {
        description = mdDoc "Whether to expose an internal library.";
        type = bool;
        default = true;
      };

      dep = {

        minor = mkOption {
          description = mdDoc ''
          Dependency string for referencing this library with its version from other Cabal package.
          Like [](#opt-package-dep.minor), but for sublibraries.
          '';
          type = util.types.hpackDep;
          readOnly = true;
        };

        exact = mkOption {
          description = mdDoc ''
          Dependency string for referencing this library with its version from other Cabal package.
          Like [](#opt-package-libraries._name_.dep.minor), but uses exact version equality, like `core ==0.4.1.0`.
          '';
          type = util.types.hpackDep;
          readOnly = true;
        };

      };

    };

    config.dep = {

      minor = {
        name = "${pkgConfig.name}:${config.name}";
        version = "^>= ${pkgConfig.cabal-config.version}";
        local = true;
      };

      exact = {
        name = "${pkgConfig.name}:${config.name}";
        version = "== ${pkgConfig.cabal-config.version}";
        local = true;
      };

    };

  };

  exeModule = sort: default: {name ? pkgName, config, ...}: {

    options = with types; {

      main = mkOption {
        type = str;
        description = mdDoc "The file name of the main module.";
        default = "Main.hs";
      };

    };

    config =
      optionalAttrs default { enable = mkDefault (!hasComponents); }
      //
      {
        dependencies = optional (config.dependOnLibrary && pkgConfig.library.enable) pkgName;
        ghc-options = mkIf (sort != "benchmark") config.ghc-options-exe;
      };

  };

  withoutVerbatim = set: removeAttrs set ["cabal"];

  component = main: src: desc: sort: suffix: single:
    types.submoduleWith {
      modules = [
        cabalOptionsModule
        (cabalComponentModule { inherit pkgName src sort desc suffix single; })
        (withoutVerbatim global.cabal)
        (withoutVerbatim global.internal.cabal-extra)
        (withoutVerbatim pkgConfig.cabal)
        main
      ];
      description = "submodule of cabal-options and cabal-component";
    };

  libSubmodule = component libModule "lib" "library" "library" null;

  exeSubmodule = default: component (exeModule "executable" default) "app" "executable" "executable" "exeSuffix";

  testSubmodule = component (exeModule "test" false) "test" "test suite" "test" "testSuffix";

  benchSubmodule = component (exeModule "benchmark" false) "benchmark" "benchmark" "benchmark" "benchSuffix";

  versionFromFile = let
    f = config.versionFile;
  in optionalAttrs (f != null && hasSuffix ".nix" f) { version = import "${global.base}/${f}"; };

in {

  options = with types; {

    name = mkOption {
      description = mdDoc "The name of the package, determined by the attribute name in the config.";
      type = str;
      default = name;
      readOnly = true;
    };

    src = mkOption {
      description = mdDoc "The root directory of the package.";
      type = path;
      example = literalExpression "./packages/api";
    };

    library = mkOption {
      description = mdDoc ''
      The library for this package.
      '';
      type = libSubmodule true;
      default = {};
    };

    libraries = mkOption {
      description = mdDoc ''
      The sublibraries of this package.
      Unlike [](#opt-package-library), these are treated specially by cabal.
      To depend on them, use `<pkg>:<lib>`.
      If [](#opt-package-libraries._name_.public) is set to `false`, you can only depend on them from other components
      in the same package (this is then called an internal library – default is `true`).
      '';
      type = attrsOf (libSubmodule false);
      default = {};
    };

    executable = mkOption {
      description = mdDoc ''
      The single executable for this package.
      To define multiple executables, use [](#opt-package-executables).
      '';
      type = exeSubmodule true true;
      default = {};
    };

    executables = mkOption {
      description = mdDoc ''
      Executables for this package.
      If [](#opt-package-executable) is defined, it will be added.
      '';
      type = attrsOf (exeSubmodule false false);
      default = {};
    };

    test = mkOption {
      description = mdDoc ''
      The single test suite for this package.
      To define multiple test suites, use [](#opt-package-tests).
      '';
      type = testSubmodule true;
      default = {};
    };

    tests = mkOption {
      description = mdDoc ''
      Test suites for this package.
      If [](#opt-package-test) is defined, it will be added.
      '';
      type = attrsOf (testSubmodule false);
      default = {};
    };

    benchmark = mkOption {
      description = mdDoc ''
      The single benchmark for this package.
      To define multiple benchmarks, use [](#opt-package-benchmarks).
      '';
      type = benchSubmodule true;
      default = {};
    };

    benchmarks = mkOption {
      description = mdDoc ''
      Benchmarks for this package.
      If [](#opt-package-benchmark) is defined, it will be added.
      '';
      type = attrsOf (benchSubmodule false);
      default = {};
    };

    rootModule = mkOption {
      description = mdDoc ''
      A convenience option that is used to generate a Hackage link.
      It should denote the module that represents the most high-level API of the package, if applicable.
      The default is to replace dashes in the name with dots.
      '';
      type = str;
    };

    hackageLink = mkOption {
      description = mdDoc ''
      A convenience option containing the URL to the Hackage page using the package name.
      '';
      type = str;
    };

    hackageRootLink = mkOption {
      description = mdDoc ''
      A convenience option containing the URL to the root module's documentation on Hackage using the package name and
      [](#opt-package-rootModule).
      '';
      type = str;
    };

    description = mkOption {
      description = mdDoc ''
      The Cabal description of this packages.
      The default is a link to the [](#opt-package-rootModule) on Hackage, using the option
      [](#opt-package-hackageRootLink).
      May be `null` to omit it from the config.
      '';
      type = nullOr str;
      default = null;
    };

    versionFile = mkOption {
      description = mdDoc ''
      The version file for this package, defaulting to the global [](#opt-hackage-hackage.versionFile) if `null`.
      When generating Cabal files, the version field will be set to the content of this file, unless
      [](#opt-cabal-version) is set explicitly.
      When bumping the version of a package with `nix run .#release`, this file is updated.
      Should be relative to the project root.
      '';
      type = nullOr str;
      default = null;
    };

    buildInputs = mkOption {
      description = mdDoc "Additional non-Haskell dependencies required by this package.";
      type = either (functionTo (listOf package)) (listOf package);
      default = [];
    };

    override = mkOption {
      description = mdDoc ''
      Manipulate the package's derivation using the combinators described in [](#overrides-combinators).
      '';
      type = functionTo (functionTo unspecified);
      default = _: id;
    };

    cabal = mkOption {
      type = deferredModule;
      description = mdDoc ''
      Cabal options that are applied to all components.

      **Note**: In order to enable cascading of these options, the definitions are not evaluated in-place, but when
      evaluating components. Therefore, referring to these values with e.g.
      `config.packages.name.cabal.version` does not work as expected if the value uses an option property like `mkIf` or
      `mkOverride`.
      You can use [](#opt-package-cabal-config) for this purpose, though.
      '';
      default = {};
    };

    cabal-config = mkOption {
      type = submoduleWith {
        modules = [
          cabalOptionsModule
          global.internal.cabal-extra
          global.cabal
          config.cabal
          versionFromFile
        ];
        description = "submodule of cabal-options";
      };
      readOnly = true;
      description = mdDoc ''
      Evaluated version of [](#opt-package-cabal), for referencing in other config values.
      May not be set by the user.
      '';
      default = {};
    };

    internal.componentsSet = mkOption {
      description = mdDoc "Internal option";
      type = attrsOf unspecified;
    };

    subpath = mkOption {
      description = mdDoc "The computed relative path of the package root directory.";
      type = str;
      readOnly = true;
    };

    dep = {

      minor = mkOption {
        description = mdDoc ''
        Dependency string for referencing this package with its version from other Cabal package.
        Uses the minor version dependency bound, strictly greater than the precise version.

        ```
        {config, ...}: {
          packages = {
            core = { version = "0.4.1.0"; };
            api = {
              dependencies = [config.packages.core.dep.minor];
            };
          }
        }
        ```

        This results in the dependency string `core >= 0.4.1.0 && < 0.5` in the Cabal file.

        Also works when using [](#opt-package-versionFile).
        '';
        type = util.types.hpackDep;
        readOnly = true;
      };

      exact = mkOption {
        description = mdDoc ''
        Dependency string for referencing this package with its version from other Cabal package.
        Like [](#opt-package-dep.minor), but uses exact version equality, like `core ==0.4.1.0`.
        '';
        type = util.types.hpackDep;
        readOnly = true;
      };

    };

  };

  config = {

    rootModule = mkDefault (concatMapStringsSep "." util.toTitle (splitString "-" config.name));

    hackageLink = mkDefault "https://hackage.haskell.org/package/${config.name}";

    hackageRootLink = mkDefault "${config.hackageLink}/docs/${replaceStrings ["."] ["-"] config.rootModule}.html";

    description = mkDefault "See ${config.hackageRootLink}";

    # TODO This adds multi-component sets unconditionally, suggesting that downstream check the enable flag, but checks
    # it itself for the single components...that doesn't seem right.
    internal.componentsSet =
      optionalAttrs config.library.enable { library = config.library; } //
      config.libraries //
      optionalAttrs config.executable.enable { ${config.executable.name} = config.executable; } //
      config.executables //
      optionalAttrs config.test.enable { ${config.test.name} = config.test; } //
      config.tests //
      optionalAttrs config.benchmark.enable { ${config.benchmark.name} = config.benchmark; } //
      config.benchmarks
      ;

    subpath = util.packageSubpath global.base config.src;

    dep = {

      minor = {
        name = config.name;
        version = "^>= ${config.cabal-config.version}";
        local = true;
      };

      exact = {
        name = config.name;
        version = "== ${config.cabal-config.version}";
        local = true;
      };

    };

  };

}
