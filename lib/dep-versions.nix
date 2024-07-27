{config, lib, util, env}:
with lib;
let
  # TODO use ghc-pkg to query versions so core library versions are accessible
  # even better: use CLI

  inherit (util.console) s indent;
  inherit (s) colors;

  pkgs = config.pkgs;

  ghc = config.envs.${env}.ghc.ghc;

  dep = spec: let
    inherit (util.version.normalize spec) name version;
    desc = if version == null then colors.magenta name else "${colors.magenta name} ${colors.cyan version}";
    pkg = ghc.${name};
    actual =
      if pkg == null
      then "[core library or unknown]"
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

in util.zscript "print-dep-versions" ''
  echo -e "$(cat ${packagesDeps})"
''
