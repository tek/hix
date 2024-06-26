{util}: let

  inherit (util) config lib;

  targets = env: if env.packages == null then config.internal.packageNames else env.packages;

  waitScript = env: let
    waitSeconds = toString env.wait;
    port = env.hostPorts.hix-internal-env-wait;
  in ''
  running=0
  echo ">>> Waiting ${waitSeconds} seconds for VM to boot..." >&2
  timeout=$(( $SECONDS + ${waitSeconds} ))
  while (( $SECONDS < $timeout ))
  do
    pong=$(${config.pkgs.socat}/bin/socat -T 1 - TCP:localhost:${toString port} <<< 'ping' 2>&1)
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
    echo ">>> VM wasn't ready after ${waitSeconds} seconds." >&2
    exit 1
  fi
  '';

  exposed = purpose: env: let
    allow = name: config.packages.${name}.expose.${purpose};
  in lib.filter allow (targets env);

  derivations = purpose: envName: let
    env = config.envs.${envName};
    ghc = env.ghc.ghc;
  in lib.genAttrs (exposed purpose env ++ config.output.extraPackages) (n: ghc.${n} // { inherit ghc; });

in {
  inherit targets waitScript derivations;
}
