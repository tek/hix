{ lib, config, withModules, ... }:
with builtins;
with lib;
with types;
let
  inherit (config.devGhc) nixpkgs pkgs;

  vanillaGhc = config.devGhc.vanillaGhc;

  ghcidLib = import ../lib/ghcid/default.nix { inherit lib config withModules; };

  vmConfig = import ./vm.nix config;

  defaultRunArgs = {
    pkg = config.main;
    module = "Main";
    name = "main";
    type = "test";
    runner = "generic";
  };

  runConfig = submodule ({ config, ... }: {
    options = {

      env = mkOption {
        type = attrsOf (either int str);
        description = "Verbatim environment variables for the ghcid process.";
        default = {};
      };

      buildInputs = mkOption {
        type = listOf package;
        description = "Packages to add to the shell environment.";
        default = [];
      };

      haskellPackages = mkOption {
        type = functionTo (listOf package);
        description = "Haskell packages to make available to Cabal and GHCi.";
        default = _: [];
      };

      search = mkOption {
        type = listOf str;
        description = "Search paths for GHCid to compile and watch files.";
        default = [];
      };

      restarts = mkOption {
        type = listOf str;
        description = "Files that should trigger a restart of GHCid, like '.cabal' files.";
        default = [];
      };

      preCommand = mkOption {
        description = "Shell command that should be executed before GHCi (on every reload).";
        type = lines;
        default = "";
      };

      preStartCommand = mkOption {
        type = lines;
        description = "Shell command that should be executed before `ghcid` (once).";
        default = "";
      };

      exitCommand = mkOption {
        type = lines;
        description = "Shell command that should be executed after `ghcid` exits.";
        default = "";
      };

      vm = mkOption {
        type = submodule (vmConfig config);
        description = "Configuration for a `qemu` VM that is started before and stopped after the command runs.";
        default = {};
      };

    };
  });

  ghcidCommand = submodule {
    options = {

      shellConfig = mkOption {
        description = "This command's nix shell configuration.";
        type = runConfig;
        default = {};
      };

      script = mkOption {
        description = "GHCi commands to execute before running the test.";
        type = lines;
      };

      test = mkOption {
        description = "The expression that is evaluated repeatedly.";
        type = str;
      };

    };
  };

in {
  options.ghcid = {

    shellConfig = mkOption {
      description = "The shell configuration shared by all commands and shells.";
      type = runConfig;
      default = {};
    };

    testConfig = mkOption {
      description = "The shell configuration shared by all tests.";
      type = functionTo runConfig;
      default = {};
    };

    commands = mkOption {
      description = "The ghcid commands exposed as flake apps.";
      type = attrsOf ghcidCommand;
      default = {};
    };

    shells = mkOption {
      description = "The shells generated for the commands.";
      type = unspecified;
    };

    shell = mkOption {
      description = "The default shell generated for use by the flake output <literal>devShell</literal>.";
      type = unspecified;
    };

    apps = mkOption {
      description = "The flake apps generated for the commands.";
      type = unspecified;
    };

    hls = mkOption {
      description = "The package for HLS.";
      type = package;
    };

    hlsApp = mkOption {
      description = "The flake app generated for HLS.";
      type = unspecified;
    };

    run = mkOption {
      description = "Internal function used to run ghcid tests.";
      type = unspecified;
    };

    easy-hls = mkOption {
      description = "Wether to use easy-hls to obtain HLS.";
      type = bool;
      default = false;
    };

    test = mkOption {
      description = "The generated app that runs a ghcid test.";
      type = functionTo unspecified;
    };

  };

  config.ghcid = {
    testConfig = mkDefault (_: config.ghcid.shellConfig);

    shells = mkDefault (mapAttrs ghcidLib.shell.runShell config.ghcid.commands);

    apps = mkDefault (mapAttrs ghcidLib.shell.app config.ghcid.commands);

    hls = mkDefault (
      if config.ghcid.easy-hls
      then config.inputs.easy-hls.defaultPackage.${config.system}
      else vanillaGhc.haskell-language-server
    );

    hlsApp = mkDefault (pkgs.writeScript "hls" "nix develop -c haskell-language-server");

    run = mkDefault (makeOverridable ghcidLib.shell.run defaultRunArgs);

    shell = mkDefault (ghcidLib.shell.shellWith { inherit (config.ghcid) shellConfig; });

    test = mkDefault (args: ghcidLib.test ({ inherit pkgs; } // args));
  };
}
