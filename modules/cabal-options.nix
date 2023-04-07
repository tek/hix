{ global, util, }:
{ lib, config, ... }:
with lib;

let

  preludeModule = {

    options = with types; {
      enable = mkEnableOption (mdDoc "the alternative Prelude");

      package = mkOption {
        description = mdDoc "The package containing the alternative Prelude.";
        type = util.types.cabalDep;
        example = literalExpression ''"relude"'';
      };

      module = mkOption {
        description = mdDoc "The module name of the alternative Prelude.";
        type = str;
        default = "Prelude";
        example = literalExpression ''"Relude"'';
      };
    };

  };

in {

  options = with types; {

    license = mkOption {
      description = mdDoc ''
      The license for all packages in this option tree.
      May be `null` to omit it from the config.
      '';
      type = nullOr str;
      default = "GPL-3";
    };

    license-file = mkOption {
      description = mdDoc ''
      The name of the file containing the license text for all packages in this option tree.
      May be `null` to omit it from the config.
      '';
      type = nullOr str;
      default = null;
    };

    version = mkOption {
      description = mdDoc ''
      The version for all packages in this option tree.
      '';
      type = str;
      default = let
        check = f: f != null && hasSuffix ".nix" f;
        fromFile = f: import "${global.base}/${f}";
      in
        if config ? versionFile && check config.versionFile
        then fromFile config.versionFile
        else if check global.hackage.versionFile
        then fromFile global.hackage.versionFile
        else "0.1.0.0";
    };

    author = mkOption {
      description = mdDoc ''
      The author of the packages in this option tree.
      May be `null` to omit it from the config.
      '';
      type = nullOr str;
      default = null;
    };

    copyrightYear = mkOption {
      description = mdDoc ''
      The year for the copyright string.
      '';
      type = str;
      default = "2023";
    };

    copyright = mkOption {
      description = mdDoc ''
      The copyright string for the packages in this option tree.
      The default is to combine [](#opt-cabal-copyrightYear) and {option}`author`;
      May be `null` to omit it from the config.
      '';
      type = nullOr str;
      default = null;
    };

    build-type = mkOption {
      description = mdDoc ''
      The build type for the packages in this option tree.
      May be `null` to omit it from the config.
      '';
      type = nullOr str;
      default = "Simple";
    };

    ghc-options = mkOption {
      description = mdDoc "GHC options for all components in this option tree.";
      type = listOf str;
      example = literalExpression ''["-Wunused-imports" "-j6"]'';
      default = [];
    };

    ghc-options-exe = mkOption {
      description = mdDoc ''
      GHC options for all executables in this option tree.
      The purpose of this is to allow [](#opt-cabal-ghc-options) to use it as the default for executables without requiring
      complicated overrides to disable it.
      If you don't want to use these options, set this option to `[]` instead of forcing other values in
      [](#opt-cabal-ghc-options).
      These options are not used for benchmarks.
      '';
      type = listOf str;
      default = [
        "-threaded"
        "-rtsopts"
        "-with-rtsopts=-N"
      ];
    };

    default-extensions = mkOption {
      description = mdDoc "GHC extensions for all components in this option tree.";
      type = listOf str;
      example = literalExpression ''["DataKinds" "FlexibleContexts" "OverloadedLists"]'';
      default = [];
    };

    language = mkOption {
      description = mdDoc ''
      The default extension set used for all components in this option tree.
      It is set to `GHC2021` if the GHC versions of all defined envs are 9.2 or greater, and `Haskell2010` otherwise.
      '';
      type = str;
      default =
        if util.minGhcs "9.2"
        then "GHC2021"
        else "Haskell2010";
    };

    dependencies = mkOption {
      description = mdDoc "Cabal dependencies used for all components in this option tree.";
      type = listOf util.types.hpackDep;
      example = literalExpression ''["aeson" "containers"]'';
      default = [];
    };

    base = mkOption {
      description = mdDoc "The dependency spec for the `base` package.";
      type = util.types.cabalDep;
      default = "base >= 4 && < 5";
    };

    baseHide = mkOption {
      description = mdDoc "The dependency spec for the `base` package used when [](#opt-cabal-prelude) is set.";
      type = util.types.cabalDep;
      default = {
        name = "base";
        version = ">= 4 && < 5";
        mixin = ["hiding (Prelude)"];
      };
    };

    prelude = mkOption {
      description = mdDoc "Configure an alternative Prelude package.";
      type = nullOr (submodule preludeModule);
      default = {};
    };

    paths = mkOption {
      description = mdDoc ''
      Cabal generates the module `Paths_packagename` for each component, which provides access to data
      files included in a package, but is rarely used.
      This may cause trouble if [](#opt-cabal-prelude) is configured to use an alternative Prelude that does not export some
      of the names used in this module.
      Setting this option to `false` prevents this module from being generated.
      '';
      type = bool;
      default = !config.prelude.enable;
    };

    dependOnLibrary = mkOption {
      description = mdDoc ''
      Convenience feature that automatically adds a dependency on the library component to all executable components, if
      the library exists.
      '';
      type = bool;
      default = true;
    };

    testSuffix = mkOption {
      description = mdDoc ''
      This string is appended to the package name to form the single test component.
      For example, given the config:
      ```
      {
        packages.spaceship = {
          test.cabal.testSuffix = "-integration";
        }
      }
      ```
      The name of the generated `testsuite` will be `spaceship-integration`.
      '';
      type = str;
      default = "-test";
    };

    exeSuffix = mkOption {
      description = mdDoc ''
      This string is appended to the package name to form the single executable component.
      See [](#opt-cabal-testSuffix) for an example.
      The default is to use no suffix, resulting in the same name as the package and library.
      '';
      type = str;
      default = "";
    };

    benchSuffix = mkOption {
      description = mdDoc ''
      This string is appended to the package name to form the single benchmark component.
      See [](#opt-cabal-testSuffix) for an example.
      '';
      type = str;
      default = "-bench";
    };

    meta = mkOption {
      description = mdDoc ''
      Verbatim top-level Cabal configuration in [HPack](https://github.com/sol/hpack) format.

      Cascades down into all packages.

      This should only be used for keys that have no corresponding module option, otherwise the values defined in a
      package might be overridden by option definitions in the global config.
      '';
      type = attrsOf unspecified;
      default = {};
    };

    cabal = mkOption {
      description = mdDoc ''
      Verbatim Cabal configuration in [HPack](https://github.com/sol/hpack) format.

      Cascades down into all packages and components.

      ::: {.note}
      This unconditionally overrides all option definitions with the same keys if they are not mergeable (like lists and
      attrsets).
      :::
      '';
      type = attrsOf unspecified;
      default = {};
    };

  };

  config = {
    copyright = mkIf (config.author != null) (mkDefault "${config.copyrightYear} ${config.author}");
  };

}
