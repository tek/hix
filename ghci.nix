{
  pkgs,
  basicArgs ? [],
  extraArgs ? [],
  commandArgs ? [],
  options_ghc ? null,
  ...
}:
let
  inherit (pkgs.lib.lists) optional;
  inherit (pkgs.lib.strings) optionalString;

  libDir = pkg: "$PWD/${pkg}/lib";

  colonSeparated =
    builtins.concatStringsSep ":";

  searchPaths =
    paths:
    "-i${colonSeparated paths}";

  preproc_options_ghc =
    if (builtins.isNull options_ghc || options_ghc == "") then [] else ["-optF" options_ghc];

  preludeScript =
    ''
      :load Prelude
      import Prelude
      :set -XImplicitPrelude
    '';

  cwdScript = cwd:
  optionalString (cwd != null) ''
    :cd ${cwd}
  '';

in rec {
  args = {
    basic = prelude:
    ["-no-user-package-db" "-package-env" "-"] ++ basicArgs ++ extraArgs ++ optional prelude "-XNoImplicitPrelude";

    command =
      commandArgs;

    preprocessor =
      ["-F" "-pgmF" ./preprocessor.bash] ++ preproc_options_ghc;
  };

  scripts = rec {

    property = module: ''
      :load ${module}
      import ${module}
      import Hedgehog (check)
    '';

    unit = cwd: module: {
      inherit cwd;
      script = ''
        :load ${module}
        import ${module}
        import Hedgehog (check, property, test, withTests)
      '';
    };

    tastyUnit = cwd: module: {
      inherit cwd;
      script = ''
        :load ${module}
        import ${module}
        import Test.Tasty (defaultMain)
      '';
    };

    generic = cwd: module: {
      inherit cwd;
      script = ''
        :load ${module}
        import ${module}
      '';
    };

    run = pkg: module: runner:
    if runner == "hedgehog-property"
    then property module
    else if runner == "hedgehog-unit"
    then unit pkg module
    else if runner == "tasty-tree"
    then tastyUnit pkg module
    else generic pkg module;

  };

  tests = {
    test =
      name: runner:
      if runner == "hedgehog-property"
      then "check ${name}"
      else if runner == "hedgehog-unit"
      then "(check . withTests 1 . property . test) ${name}"
      else if runner == "tasty-tree"
      then "defaultMain ${name}"
      else name;
  };

  prePreludeScript = prelude: ''
    :load ${prelude}
    import Prelude
    :add Prelude
  '';

  ghciScript = cwd: prelude: script:
  let
    scriptText = if builtins.isAttrs script then script.script else script;
  in
    cwdScript cwd + optionalString prelude preludeScript + scriptText;

  command = {
    packages,
    script,
    extraSearch,
    prelude ? true,
  }:
    let
      cwd = if builtins.isAttrs script && script ? cwd then script.cwd else null;
      basic = toString (args.basic prelude);
      command = toString args.command;
      preproc = toString args.preprocessor;
      search = searchPaths ((map libDir (builtins.attrValues packages)) ++ extraSearch);
      scriptFile = pkgs.writeText "ghci-script" (ghciScript cwd prelude script);
    in
    "ghci ${basic} ${command} ${preproc} ${search} -ghci-script ${scriptFile}";
}
