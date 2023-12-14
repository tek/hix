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

      update = mkOption {
        description = mdDoc ''
        Whether to build the project with the new latest versions and update [](#opt-managed-managed.file) on success.
        Not applicable for [lower bounds](#opt-managed-managed.lower.enable) – there would be nothing to do if this were
        disabled.
        '';
        type = bool;
        default = true;
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

      # TODO probably better to do this in CI
      commit = mkOption {
        description = mdDoc "";
        type = bool;
        default = false;
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

      lower = {

        enable = mkOption {
          description = mdDoc "Enable an environment for testing lower bounds.";
          type = bool;
          default = false;
        };

        solverBounds = mkOption {
          description = mdDoc ''
          Extra package bounds that Cabal's dependency solver should always use when finding a working set.
          This can be used to avoid problematic versions that have dependencies with a high tendency to break the build.
          '';
          type = attrsOf str;
          default = {};
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

    };
  };
}
