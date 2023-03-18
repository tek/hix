{ util, }:
{ lib, config, ... }:
with lib;

let

  hixtypes = import ../lib/types.nix { inherit lib; };

  preludeModule = {

    options = with types; {
      enable = mkEnableOption "the alternative Prelude";

      package = mkOption {
        description = "The package containing the alternative Prelude.";
        type = util.types.cabalDep;
      };

      module = mkOption {
        description = "The module name of the alternative Prelude.";
        type = str;
        default = "Prelude";
      };
    };

  };

in {

  options = with types; {

    license = mkOption {
      description = ''
      The license for all packages in this option tree.
      May be `null` to omit it from the config.
      '';
      type = nullOr str;
      default = "GPL-3";
    };

    license-file = mkOption {
      description = ''
      The name of the file containing the license text for all packages in this option tree.
      May be `null` to omit it from the config.
      '';
      type = nullOr str;
      default = null;
    };

    version = mkOption {
      description = ''
      The version for all packages in this option tree.
      '';
      type = str;
      default = "0.1.0.0";
    };

    author = mkOption {
      description = ''
      The author of the packages in this option tree.
      May be `null` to omit it from the config.
      '';
      type = nullOr str;
      default = null;
    };

    copyrightYear = mkOption {
      description = ''
      The year for the copyright string.
      '';
      type = str;
      default = "2023";
    };

    copyright = mkOption {
      description = ''
      The copyright string for the packages in this option tree.
      The default is to combine {option}`copyrightYear` and {option}`author`;
      May be `null` to omit it from the config.
      '';
      type = nullOr str;
      default = null;
    };

    build-type = mkOption {
      description = ''
      The build type for the packages in this option tree.
      May be `null` to omit it from the config.
      '';
      type = nullOr str;
      default = "Simple";
    };

    ghc-options = mkOption {
      description = "GHC options for all components in this option tree.";
      type = listOf str;
      example = literalExpression ''["-Wunused-imports" "-j6" "-XGHC2021"]'';
      default = [];
    };

    ghc-options-exe = mkOption {
      description = ''
      GHC options for all executables in this option tree.
      The purpose of this is to allow {option}`ghc-options` to use it as the default for executables without requiring
      complicated overrides to disable it.
      If you don't want to use these options, set this option to `[]` instead of forcing other values in
      {option}`ghc-options`.
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
      description = "GHC extensions for all components in this option tree.";
      type = listOf str;
      example = literalExpression ''["DataKinds" "FlexibleContexts" "OverloadedLists"]'';
      default = [];
    };

    dependencies = mkOption {
      description = "Cabal dependencies used for all components in this option tree.";
      type = listOf (either str (attrsOf unspecified));
      example = literalExpression ''["aeson" "containers"]'';
      default = [];
    };

    base = mkOption {
      description = "The dependency spec for the <literal>base</literal> package.";
      type = hixtypes.cabalDep;
      default = "base >= 4 && < 5";
    };

    baseHide = mkOption {
      description = "The dependency spec for the <literal>base</literal> package used when {option}`prelude` is set.";
      type = hixtypes.cabalDep;
      default = {
        name = "base";
        version = ">= 4 && < 5";
        mixin = ["hiding (Prelude)"];
      };
    };

    prelude = mkOption {
      description = "Configure an alternative Prelude package.";
      type = nullOr (submodule preludeModule);
      default = null;
    };

    paths = mkOption {
      description = ''
      Cabal generates the module <literal>Paths_packagename</literal> for each component, which provides access to data
      files included in a package, but is rarely used.
      This may cause trouble if {option}`prelude` is configured to use an alternative Prelude that does not export some
      of the names used in this module.
      Setting this option to <literal>false</literal> prevents this module from being generated.
      '';
      type = bool;
      default = true;
    };

    dependOnLibrary = mkOption {
      description = ''
      Convenience feature that automatically adds a dependency on the library component to all executable components, if
      the library exists.
      '';
      type = bool;
      default = true;
    };

    testSuffix = mkOption {
      description = ''
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
      description = ''
      This string is appended to the package name to form the single executable component.
      See {option}`testSuffix` for an example.
      The default is to use no suffix, resulting in the same name as the package and library.
      '';
      type = str;
      default = "";
    };

    benchSuffix = mkOption {
      description = ''
      This string is appended to the package name to form the single benchmark component.
      See {option}`testSuffix` for an example.
      '';
      type = str;
      default = "-bench";
    };

    meta = mkOption {
      description = ''
      Verbatim top-level Cabal configuration in [HPack](https://github.com/sol/hpack) format.
      Cascades down into all packages.
      This should only be used for keys that have no corresponding module option, otherwise the values defined in a
      package might be overridden by option definitions in the global config.
      '';
      type = attrsOf unspecified;
      default = {};
    };

    cabal = mkOption {
      description = ''
      Verbatim Cabal configuration in [HPack](https://github.com/sol/hpack) format.
      Does not cascade into packages and components.
      **Note**: This unconditionally overrides all option definitions with the same keys if they are not mergeable (like
      lists and attrsets).
      '';
      type = attrsOf unspecified;
      default = {};
    };

  };

  config = {
    copyright = mkIf (config.author != null) (mkDefault "${config.copyrightYear} ${config.author}");
  };

}
