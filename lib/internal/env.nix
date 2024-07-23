{util}: let

  inherit (util) config internal lib;

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

  withEnv = f: envName: f config.envs.${envName};

  envMainPackage = env: let
    targets = internal.env.targets env;
  in
    if lib.elem config.main targets
    then config.main
    else internal.packages.selectMain targets;

  withMain = alt: f: env:
  util.maybe alt (pkgName: f config.packages.${pkgName}) (envMainPackage env);

  setWithMain = withMain {};

  withExe = alt: f:
  withMain alt (main: internal.package.withExe alt (f main) main);

  setWithExe = withExe {};

  prefixedWith = envName: util.mapKeys (n: _: "${envName}-${n}");

  prefixed = lib.concatMapAttrs prefixedWith;

  isExposed = purpose: env:
  if isNull env
  then false
  else
  if lib.isAttrs env.expose
  then env.expose.${purpose}
  else env.expose;

  justExposed = purpose: env: a:
  if isExposed purpose env then a else null;

  filterExposed = purpose:
  internal.packages.mapMaybe (internal.package.justExposed purpose);

  targets = env: if env.packages == null then config.internal.packageNames else env.packages;

  isTarget = envName: pkgName:
  lib.elem pkgName (withEnv targets envName);

in {
  inherit
  waitScript
  withEnv
  withMain
  setWithMain
  withExe
  setWithExe
  prefixedWith
  prefixed
  isExposed
  justExposed
  filterExposed
  targets
  isTarget
  ;
}
