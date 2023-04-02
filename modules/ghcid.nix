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

  flakeAppWithEnv = env: config.pkgs.writeScript "ghcid-env-run" ''
  #!${config.pkgs.bashInteractive}/bin/bash
  set -eu
  config=$(cat ${config.ghci.cliJson})
  ghcid_cmd=$(${cli} ghcid-cmd -c "$config" $@)
  ${env.runner} "eval $ghcid_cmd"
  '';

  flakeApp = config.pkgs.writeScript "ghcid-run" ''
  #!${config.pkgs.bashInteractive}/bin/bash
  set -eu
  config=$(cat ${config.ghci.cliJson})
  env_runner=$(${cli} component-env -c "$config" $@)
  ghcid_cmd=$(${cli} ghcid-cmd -c "$config" $@)
  $env_runner "eval $ghcid_cmd"
  '';

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

      vm = mkOption {
        type = submodule (vmConfig config);
        description = mdDoc "Configuration for a `qemu` VM that is started before and stopped after the command runs.";
        default = {};
      };

    };
  });

  ghcidCommand = submodule {
    options = {

      shellConfig = mkOption {
        description = mdDoc "This command's nix shell configuration.";
        type = runConfig;
        default = {};
      };

      script = mkOption {
        description = mdDoc "GHCi commands to execute before running the test.";
        type = lines;
      };

      test = mkOption {
        description = mdDoc "The expression that is evaluated repeatedly.";
        type = str;
      };

    };
  };

in {
  options.ghcid = {

    shellConfig = mkOption {
      description = mdDoc "The shell configuration shared by all commands and shells.";
      type = runConfig;
      default = {};
    };

    testConfig = mkOption {
      description = mdDoc "The shell configuration shared by all tests.";
      type = functionTo runConfig;
      default = {};
    };

    commands = mkOption {
      description = mdDoc "The ghcid commands exposed as flake apps.";
      type = attrsOf ghcidCommand;
      default = {};
    };

    shells = mkOption {
      description = mdDoc "The shells generated for the commands.";
      type = unspecified;
    };

    shell = mkOption {
      description = mdDoc "The default shell generated for use by the flake output `devShell`.";
      type = unspecified;
    };

    apps = mkOption {
      description = mdDoc "The flake apps generated for the commands.";
      type = unspecified;
    };

    run = mkOption {
      description = mdDoc "Internal function used to run ghcid tests.";
      type = unspecified;
    };

    test = mkOption {
      description = mdDoc "The generated app that runs a ghcid test.";
      type = functionTo unspecified;
    };

    lib = mkOption {
      description = mdDoc "The internal logic for devshells and ghcid.";
      type = unspecified;
    };

    flakeApp = mkOption {
      description = "";
      type = path;
      default = flakeApp;
      readOnly = true;
    };

    flakeAppWithEnv = mkOption {
      description = "";
      type = functionTo path;
      default = flakeAppWithEnv;
      readOnly = true;
    };

  };

  config.ghcid = {
    testConfig = mkDefault (_: config.ghcid.shellConfig);

    shells = mkDefault (mapAttrs ghcidLib.shell.runShell config.ghcid.commands);

    apps = mkDefault (mapAttrs ghcidLib.shell.app config.ghcid.commands);

    run = mkDefault ghcidLib.shell.run;

    shell = mkDefault (ghcidLib.shell.shellWith { inherit (config.ghcid) shellConfig; });

    lib = ghcidLib;
  };
}
