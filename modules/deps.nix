{lib, ...}:
with lib;
{
  options = with types; {
    managedDeps = {

      enable = mkOption {
        description = mdDoc "Enable managed dependencies.";
        type = bool;
        default = false;
      };

      file = mkOption {
        description = mdDoc "Relative path to the file in which dependency versions should be stored.";
        type = str;
        default = "ops/deps.nix";
      };

      update = mkOption {
        description = mdDoc ''
        Whether to build the project with the new dependency versions and update [](#opt-general-managedDeps.file) on
        success.
        '';
        type = bool;
        default = true;
      };

      generate = mkOption {
        description = mdDoc ''
        Whether to regenerate cabal files and override derivations after [updating](#opt-general-managedDeps.update) the
        project.
        '';
        type = bool;
        default = true;
      };

      check = mkOption {
        description = mdDoc "Add the build with latest versions to the flake checks.";
        type = bool;
        default = true;
      };

      verbose = mkOption {
        description = mdDoc "Print verbose messages when building packages with new versions.";
        type = bool;
        default = false;
      };

      debug = mkOption {
        description = mdDoc "Print debug messages when bumping dependencies.";
        type = bool;
        default = false;
      };

    };
  };
}
