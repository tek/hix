{ lib, config, util, ... }:
with builtins;
with lib;
with types;
let
  inherit (config.envs.dev.ghc) pkgs;

  vanillaGhc = config.envs.dev.ghc.vanillaGhc;

  ghcidLib = import ../lib/ghcid/default.nix { inherit lib config util; };

  vmConfig = import ./vm.nix config;

  cli = config.internal.hixCli.exe;

  runConfig = submodule ({ config, ... }: {
    options = {

      env = mkOption {
        type = attrsOf (either int str);
        description = mdDoc "Verbatim environment variables for the ghcid process.";
        default = {};
      };

      buildInputs = mkOption {
        type = listOf package;
        description = mdDoc "Packages to add to the shell environment.";
        default = [];
      };

      haskellPackages = mkOption {
        type = functionTo (listOf package);
        description = mdDoc "Haskell packages to make available to Cabal and GHCi.";
        default = _: [];
      };

      search = mkOption {
        type = listOf str;
        description = mdDoc "Search paths for GHCid to compile and watch files.";
        default = [];
      };

      restarts = mkOption {
        type = listOf str;
        description = mdDoc "Files that should trigger a restart of GHCid, like '.cabal' files.";
        default = [];
      };

      preCommand = mkOption {
        description = mdDoc "Shell command that should be executed before GHCi (on every reload).";
        type = lines;
        default = "";
      };

      preStartCommand = mkOption {
        type = lines;
        description = mdDoc "Shell command that should be executed before `ghcid` (once).";
        default = "";
      };

      exitCommand = mkOption {
        type = lines;
        description = mdDoc "Shell command that should be executed after `ghcid` exits.";
        default = "";
      };

    };
  });

in {
  options.ghcid = {

    shells = mkOption {
      description = mdDoc "The shells generated for the commands.";
      type = unspecified;
    };

    shell = mkOption {
      description = mdDoc "The default shell generated for use by the flake output `devShell`.";
      type = unspecified;
    };

    lib = mkOption {
      description = mdDoc "The internal logic for devshells and ghcid.";
      type = unspecified;
    };

  };

  config.ghcid = {
    shells = mkDefault (mapAttrs ghcidLib.shell.runShell config.ghcid.commands);

    shell = mkDefault (ghcidLib.shell.shellWith { inherit (config.ghcid) shellConfig; });

    lib = ghcidLib;
  };

  config.commands.ghcid = {

    command = ''
    config=$(cat ${util.json.ghciFile})
    ghcid_cmd=$(${cli} ghcid-cmd -c "$config" ''${env_args[@]} ''${cmd_args[@]})
    env_run "eval $ghcid_cmd"
    '';

  };
}
