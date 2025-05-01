{util}:
let
  inherit (util) config pkgs lib;

  spec = import ./deps/spec.nix { inherit lib; };
  deps = import ./deps/default.nix { inherit config; } { inherit pkgs; };
  inherit (util.console) color bold indent chevrons chevronY chevronM;
  inherit (util.console.colors) blue green;

  renderSource = lib.concatStringsSep (bold (color green " -> "));

  renderSpec = spec: "${chevronY} ${spec.desc}";

  decl = name: specs: ["" "${chevronY} Package ${color blue name}"] ++ indent (map renderSpec (spec.listOC specs));

  declsVia = desc: specs: ["" "${chevronM} ${desc}"] ++ indent (lib.concatLists (lib.mapAttrsToList decl specs));

  declSet = self: super: specs: let
    api = import ./deps/api.nix { inherit pkgs config; } { inherit self super; };
    desc =
      if lib.isAttrs specs && specs ? __source
      then renderSource specs.__source
      else "Unknown source";
  in declsVia desc (specs api);

  # TODO this should probably remove `__all` – add it to the test
  decls = env: let
    self = env.toolchain.packages;
    super = env.toolchain.vanilla;
    individual = lib.concatMap (declSet self super) env.toolchain.overrides;
    combined = declsVia "combined" (deps.normalize env.toolchain.overrides self super);
  in indent (individual ++ combined);

  showEnv = env: ["${chevrons} Environment ${color green env.name}"] ++ decls env ++ [""];

  dumpEnv = _: env: pkgs.writeText "show-overrides-${env.name}" (util.unlines (showEnv env));

  envs = lib.mapAttrs dumpEnv config.envs;

  envA = n: t: "${n} ${t}";

  envsA = lib.concatStringsSep " " (lib.mapAttrsToList envA envs);

in util.zscriptErr "show-overrides" ''
  typeset -A envs
  set -A envs ${envsA}

  print_env()
  {
    env="''${envs[$1]}"
    if [[ -z $env ]]
    then
      echo -e "${chevrons} Invalid env name: $name"
    else
      echo -e "$(< $env)"
      echo ""
    fi
  }

  if [[ $# == 0 ]]
  then
    targets="''${(k)envs}"
  else
    targets="$@"
  fi

  for name in $=targets
  do
    print_env $name
  done
''
