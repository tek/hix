{config, lib, util, ...}:
with lib; let

  envConfigModule = sort: import ./managed/env.nix { global = config; inherit sort util; };

in {
  options = with types; {
    managed = {

      enable = mkOption {
        description = "Enable managed dependencies.";
        type = bool;
        default = false;
      };

      sets = mkOption {
        description = ''
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
        description = "Relative path to the file in which dependency versions should be stored.";
        type = str;
        default = "ops/managed.nix";
      };

      generate = mkOption {
        description = ''
        Whether to regenerate cabal files and override derivations after updating the project.
        '';
        type = bool;
        default = true;
      };

      check = mkOption {
        description = "Add builds with latest versions and lower bounds to the flake checks.";
        type = bool;
        default = true;
      };

      gitAdd = mkOption {
        description = ''
        Git-add [the managed deps](#opt-managed-managed.file) after the first run.
        Since nix ignores untracked files in flakes, the state wouldn't be loaded if you forgot to add the file
        yourself.
        '';
        type = bool;
        default = true;
      };

      verbose = mkOption {
        description = "Print verbose messages when managing dependencies.";
        type = bool;
        default = false;
      };

      debug = mkOption {
        description = "Print debug messages when managing dependencies.";
        type = bool;
        default = false;
      };

      quiet = mkOption {
        description = "Suppress informational messages when managing dependencies.";
        type = bool;
        default = false;
      };

      envs = mkOption {
        description = ''
        Options for environments generated for managed dependencies.
        These apply to both `latest` and `lower` environments; the modules [](#opt-managed-managed.latest.envs) and
        [](#opt-managed-managed.lower.envs) have precedence over them.
        '';
        type = submodule (envConfigModule "common");
        default = {};
      };

      mergeBounds = mkOption {
        description = ''
        Add the flake bounds to the managed bounds.
        Aside from going in the Cabal file, they are added to Cabal's dependency solver when finding new bounds.
        This can be used to avoid problematic versions that have dependencies with a high tendency to break the build.
        The ranges defined here are intersected with the managed bounds.
        If you want to relax bounds, use [](#opt-managed-managed.forceBounds).
        '';
        type = bool;
        default = false;
      };

      forceBounds = mkOption {
        description = ''
        Concrete bounds that fully override those computed by the app when generating Cabal files.
        This is useful to relax the bounds of packages that cannot be managed, like `base`, for example when the GHC
        used for the latest env isn't the newest one because the dependencies are all broken right after release, but
        you want it to build with that version anyway.
        '';
        type = attrsOf util.types.bounds;
        default = {};
        example = literalExpression ''
        {
          base = { upper = "4.21"; };
        }
        '';
      };

      latest = {

        compiler = mkOption {
          description = ''
          The GHC version (as the attribute name in `haskell.packages`) that should be used for latest versions
          environments.
          The default is to use the last entry in [](#opt-general-ghcVersions), or [](#opt-general-compiler) if the
          former is empty.
          It is advisable to use the latest GHC version that you want to support, since boot libraries will fail to
          build with different GHCs.
          '';
          type = str;
          default = if config.ghcVersions == [] then config.compiler else last config.ghcVersions;
        };

        readFlakeBounds = mkOption {
          description = "Use the upper bounds from the flake for the first run.";
          type = bool;
          default = false;
        };

        envs = mkOption {
          description = ''
          Options for environments generated for latest versions.
          These default to the values in [](#opt-managed-managed.envs).
          '';
          type = submodule (envConfigModule "latest");
          default = {};
        };

      };

      lower = {

        enable = mkOption {
          description = "Enable an environment for testing lower bounds.";
          type = bool;
          default = false;
        };

        compiler = mkOption {
          description = ''
          The GHC version (as the attribute name in `haskell.packages`) that should be used for lower bounds
          environments.
          The default is to use the first entry in [](#opt-general-ghcVersions), or [](#opt-general-compiler) if the
          former is empty.
          It is advisable to use the lowest GHC version that you want to support, since boot libraries will fail to
          build with different GHCs.
          '';
          type = str;
          default = if config.ghcVersions == [] then config.compiler else head config.ghcVersions;
        };

        envs = mkOption {
          description = ''
          Options for environments generated for lower bounds.
          These default to the values in [](#opt-managed-managed.envs).
          '';
          type = submodule (envConfigModule "latest");
          default = {};
        };

      };

      internal = {

        localsInPackageDb = mkOption {
          description =
            "Whether to include local packages as source derivations in the package db used for the solver";
          type = bool;
          default = false;
        };

      };

    };
  };
}
