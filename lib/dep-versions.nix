{config, lib, util, env}:
with lib;
let

  console = import ./console.nix { inherit lib; };
  inherit (console) s indent;
  inherit (s) colors;

  pkgs = config.pkgs;

  ghc = config.envs.${env}.ghc.ghc;

  dep = spec: let
    inherit (util.version.normalize spec) name version;
    desc = if version == null then colors.magenta name else "${colors.magenta name} ${colors.cyan version}";
    pkg = ghc.${name};
    actual =
      if pkg == null
      then "[boot package or unknown]"
      else "${colors.green "->"} ${colors.red pkg.version}";
  in ["${desc} ${actual}"];

  componentDeps = name: conf: let
    head = colors.yellow name;
    prelude = optionals conf.prelude.enable (dep conf.prelude.package);
  in ["" head] ++ indent (prelude ++ concatMap dep conf.dependencies);

  packageDeps = name: conf: let
    head = colors.blue name;
  in ["" head] ++ indent (util.concatMapAttrsToList componentDeps conf.internal.componentsSet);

  packagesDeps =
    pkgs.writeText "dep-versions" (util.unlines (util.concatMapAttrsToList packageDeps config.packages));

in pkgs.writeScript "print-dep-versions" ''
  #!${pkgs.zsh}/bin/zsh
  echo -e "$(cat ${packagesDeps})"
''
