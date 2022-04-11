{ pkgs }:
with pkgs.lib;
let
  display = vm:
  if vm.headless then "-display none" else "";

  ensure = vm: pkgs.writeScript "ensure-vm" ''
    #!${pkgs.zsh}/bin/zsh
    if ${pkgs.procps}/bin/pgrep -F ${vm.pidfile} -L -f ${vm.pidfile} &>/dev/null
    then
      print '>>> vm already running' >&2
    else
      print '>>> starting vm' >&2
      mkdir -p ${vm.dir}
      rm -f ${vm.pidfile}
      ${vm.vm}/bin/run-nixos-vm ${display vm} -daemonize -pidfile ${vm.pidfile}
    fi
  '';

  kill = vm: pkgs.writeScript "kill-vm" ''
    #!${pkgs.zsh}/bin/zsh
    pid=$(${pkgs.procps}/bin/pgrep -F ${vm.pidfile} -L -f ${vm.pidfile})
    if [[ $? == 0 ]]
    then
      print '>>> killing vm' >&2
      kill $pid
    else
      print '>>> vm not running' >&2
    fi
  '';

in {
  inherit create postgres ensure kill;
}
