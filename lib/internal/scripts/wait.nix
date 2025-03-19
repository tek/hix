{util}: env: let

  waitSeconds = toString env.wait;
  port = env.hostPorts.hix-internal-env-wait;

in util.hixScript "hix-vm-wait" { path = pkgs: [pkgs.socat]; } ''
running=0
message "Waiting ${waitSeconds} seconds for VM to boot..."
timeout=$(( $SECONDS + ${waitSeconds} ))
while (( $SECONDS < $timeout ))
do
  pong=$(socat -T 1 - TCP:localhost:${toString port} <<< 'ping' 2>&1)
  if [[ $pong == 'running' ]]
  then
    running=1
    break
  else
    sleep 0.1
  fi
done
if [[ $running == 0 ]]
then
  error_message "VM wasn't ready after ${waitSeconds} seconds."
  exit 1
fi
''
