{ lib, config, }:
with builtins;
with lib;
let
  pkgs = config.internal.basicPkgs;

  vms = import ../vm.nix { inherit pkgs; };

  startVm = vm: if vm == null then "" else vms.ensure vm;

  stopVm = vm: if vm == null then "" else vms.kill vm;

  restart = f: ''--restart="${f}"'';

in rec {
  ghcidCmd =
    command: test: restarts:
    ''
      ghcid ${toString (map restart restarts)} --command="${command}" --test='${test}'
    '';

  ghcidScript = {
    mainScript,
    exitCommand,
  }:
  ''
    #!${pkgs.zsh}/bin/zsh
    quitting=0
    quit() {
      if [[ $quitting == 0 ]]
      then
        quitting=1
        print ">>> quitting due to signal $1"
        ${exitCommand}
        # kill zombie GHCs
        ${pkgs.procps}/bin/pkill -9 -x -P 1 ghc
      fi
      return 1
    }
    TRAPINT() { quit $* }
    TRAPTERM() { quit $* }
    TRAPKILL() { quit $* }
    TRAPEXIT() { quit $* }
    ${mainScript}
  '';

  shellCommand = {
    script,
    test,
    shellConfig,
    cwd ? null,
  }:
  let

    vm = if shellConfig.vm.enable then shellConfig.vm else null;

    ghciCommand = config.ghci.command {
      packages = config.internal.relativePackages;
      inherit script cwd;
      inherit (shellConfig) search;
    };

    commands = ''
      ${shellConfig.preCommand}
      ${ghciCommand.cmdline}
    '';

    ghcidCommand = ghcidCmd commands test shellConfig.restarts;

    mainScript = ''
      ${shellConfig.preStartCommand}
      ${startVm vm}
      ${ghcidCommand}
    '';

    exitCommand = ''
      ${stopVm vm}
      ${shellConfig.exitCommand}
    '';

    scriptText = ghcidScript { inherit mainScript exitCommand; };

  in {
    inherit vm ghciCommand ghcidCommand mainScript exitCommand scriptText;
    script = pkgs.writeScript "ghcid-cmd" scriptText;
  };
}
