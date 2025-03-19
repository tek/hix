{util}:
let

  display = vm:
  if vm.headless then "-display none" else "";

  ensure = basePort: vm: util.hixScript "hix-ensure-vm" { path = pkgs: [pkgs.procps]; } ''
  if pgrep -F ${vm.pidfile} -L -f ${vm.pidfile} &> /dev/null
  then
    message 'VM already running'
  else
    message 'Starting VM with base port ${toString basePort}'
    mkdir -p ${vm.dir}
    rm -f ${vm.pidfile}
    ${vm.derivation}/bin/run-nixos-vm ${display vm} \
      -daemonize -pidfile ${vm.pidfile} \
      -monitor unix:${vm.monitor},server,nowait
  fi
  '';

  kill = vm: util.hixScript "hix-kill-vm" { path = pkgs: [pkgs.socat pkgs.procps]; } ''
  pid=$(pgrep -F ${vm.pidfile} -L -f ${vm.pidfile})
  if [[ $? == 0 ]]
  then
    message 'Shutting down VM'
    socat - UNIX-CONNECT:${vm.monitor} <<< system_powerdown &> /dev/null
  else
    error_message 'VM not running'
  fi
  '';

in {
  inherit ensure kill;
}
