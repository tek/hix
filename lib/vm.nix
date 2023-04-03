{ pkgs }:
with pkgs.lib;
let
  display = vm:
  if vm.headless then "-display none" else "";

  ensure = vm: pkgs.writeScript "ensure-vm" ''
    #!${pkgs.zsh}/bin/zsh
    if ${pkgs.procps}/bin/pgrep -F ${vm.pidfile} -L -f ${vm.pidfile} &>/dev/null
    then
      print '>>> VM already running' >&2
    else
      print '>>> Starting VM' >&2
      mkdir -p ${vm.dir}
      rm -f ${vm.pidfile}
      ${vm.derivation}/bin/run-nixos-vm ${display vm} -daemonize -pidfile ${vm.pidfile}
    fi
  '';

  kill = vm: pkgs.writeScript "kill-vm" ''
    #!${pkgs.zsh}/bin/zsh
    pid=$(${pkgs.procps}/bin/pgrep -F ${vm.pidfile} -L -f ${vm.pidfile})
    if [[ $? == 0 ]]
    then
      print '>>> Killing VM' >&2
      kill $pid
    else
      print '>>> VM not running' >&2
    fi
  '';

in {
  inherit create postgres ensure kill;
}
