{config, lib, util, ...}:
with lib;
{
  options = with types; {
    managed = {

      enable = mkOption {
        description = mdDoc "Enable managed dependencies.";
        type = bool;
        default = false;
      };

      sets = mkOption {
        description = mdDoc ''
        Select how to group packages for processing by the managed deps tool.
        `all` for a single set, `each` for one set per package, and an attrset for custom grouping.
        '';
        type = either (enum ["all" "each"]) (attrsOf (listOf util.types.localPackage));
        default = "all";
        example = literalExpression ''
        {
          main = ["core" "api" "app"];
          other = ["docs" "compat"];
        }
        '';
      };

      file = mkOption {
        description = mdDoc "Relative path to the file in which dependency versions should be stored.";
        type = str;
        default = "ops/managed.nix";
      };

      generate = mkOption {
        description = mdDoc ''
        Whether to regenerate cabal files and override derivations after [updating](#opt-managed-managed.update) the
        project.
        '';
        type = bool;
        default = true;
      };

      check = mkOption {
        description = mdDoc "Add builds with latest versions and lower bounds to the flake checks.";
        type = bool;
        default = true;
      };

      gitAdd = mkOption {
        description = mdDoc ''
        Git-add [the managed deps](#opt-managed-managed.file) after the first run.
        Since nix ignores untracked files in flakes, the state wouldn't be loaded if you forgot to add the file
        yourself.
        '';
        type = bool;
        default = true;
      };

      verbose = mkOption {
        description = mdDoc "Print verbose messages when managing dependencies.";
        type = bool;
        default = false;
      };

      debug = mkOption {
        description = mdDoc "Print debug messages when managing dependencies.";
        type = bool;
        default = false;
      };

      quiet = mkOption {
        description = mdDoc "Suppress informational messages when managing dependencies.";
        type = bool;
        default = false;
      };

      envConfig = mkOption {
        description = mdDoc ''
        Default config for environments generated for managed dependencies.
        These can be overriden per-environment by specifying `envs.lower.<attr>` like for any other environment.
        '';
        type = unspecified;
        default = {
          managed = mkDefault true;
          hide = mkDefault true;
        };
      };

      readUpperBounds = mkOption {
        description = mdDoc "Use the upper bounds from the flake for the first run.";
        type = bool;
        default = false;
      };

      mergeBounds = mkOption {
        description = mdDoc ''
        Add the flake bounds to the managed bounds.
        Aside from going in the Cabal file, they are added to Cabal's dependency solver when finding new bounds.
        This can be used to avoid problematic versions that have dependencies with a high tendency to break the build.
        '';
        type = bool;
        default = false;
      };

      lower = {

        enable = mkOption {
          description = mdDoc "Enable an environment for testing lower bounds.";
          type = bool;
          default = false;
        };

        compiler = mkOption {
          description = mdDoc ''
          The GHC version (as the attribute name in `haskell.packages`) that should be used for the lower bounds
          environment.
          The default is to use the first entry in [](#opt-general-ghcVersions), or [](#opt-general-compiler) if the
          former is empty.
          It is advisable to use the lowest GHC version that you want to support, since boot libraries will fail to
          build with newer GHCs.
          '';
          type = str;
          default = if config.ghcVersions == [] then config.compiler else head config.ghcVersions;
        };

      };

      internal = {

        localsInPackageDb = mkOption {
          description =
            mdDoc "Whether to include local packages as source derivations in the package db used for the solver";
          type = bool;
          default = false;
        };

      };

    };
  };
}
