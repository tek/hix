{config, lib, util, env}:
let
  # TODO use ghc-pkg to query versions so core library versions are accessible
  # even better: use CLI

  inherit (util) internal;

  inherit (util.console) s indent;
  inherit (s) colors;

  pkgs = config.pkgs;

  ghc = config.envs.${env}.ghc.ghc;

  dep = spec: let

    norm = util.version.normalize spec;

    name = util.version.mainLibName norm.name;

    desc = if norm.version == null then colors.magenta name else "${colors.magenta name} ${colors.cyan norm.version}";

    pkg = ghc.${name};

    actual =
      if pkg == null
      then "[core library or unknown]"
      else "${colors.green "->"} ${colors.red pkg.version}";

  in ["${desc} ${actual}"];

  componentDeps = name: conf: let
    head = colors.yellow name;
    prelude = lib.optionals conf.prelude.enable (dep conf.prelude.package);
  in ["" head] ++ indent (prelude ++ lib.concatMap dep conf.dependencies);

  packageDeps = name: conf: let
    head = colors.blue name;
  in ["" head] ++ indent (util.concatMapAttrsToList componentDeps (internal.packages.normalized conf));

  packagesDeps =
    pkgs.writeText "dep-versions" (util.unlines (util.concatMapAttrsToList packageDeps config.packages));

in util.zscriptBin "print-dep-versions" ''
  echo -e "$(cat ${packagesDeps})"
''
