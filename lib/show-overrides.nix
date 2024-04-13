{config, lib, util}:
with lib;
let
  pkgs = config.pkgs;

  spec = import ./deps/spec.nix { inherit lib; };
  deps = import ./deps/default.nix { inherit pkgs; };
  console = import ./console.nix { inherit lib; };
  inherit (console) color bold indent chevrons chevronY chevronM;
  inherit (console.colors) blue green;

  renderSource = concatStringsSep (bold (color green " -> "));

  renderSpec = spec: "${chevronY} ${spec.desc}";

  decl = name: specs: ["" "${chevronY} Package ${color blue name}"] ++ indent (map renderSpec (spec.listOC specs));

  declsVia = desc: specs: ["" "${chevronM} ${desc}"] ++ indent (concatLists (mapAttrsToList decl specs));

  declSet = ghc: specs: let
    api = import ./deps/api.nix { inherit pkgs; self = ghc; super = ghc; };
    desc =
      if isAttrs specs && specs ? __source
      then renderSource specs.__source
      else "Unknown source";
  in declsVia desc (specs api);

  decls = env: let
    ghc = env.ghc.vanillaGhc;
    individual = concatMap (declSet ghc) env.ghc.overrides;
    combined = declsVia "combined" (deps.normalize env.ghc.overrides ghc ghc);
  in indent (individual ++ combined);

  showEnv = env: ["${chevrons} Environment ${color green env.name}"] ++ decls env ++ [""];

  dumpEnv = _: env: pkgs.writeText "show-overrides-${env.name}" (util.unlines (showEnv env));

  envs = mapAttrs dumpEnv config.envs;

  envA = n: t: "${n} ${t}";

  envsA = concatStringsSep " " (mapAttrsToList envA envs);

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
