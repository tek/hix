{
  pkgs,
  basicArgs ? [],
  extraArgs ? [],
  commandArgs ? [],
  options_ghc ? null,
  testScripts ? {},
  testRunners ? {},
  ...
}:
with pkgs.lib;
let

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

  builtinTestScripts = {
    hedgehog-property = module: ''
      :load ${module}
      import ${module}
      import Hedgehog (check)
    '';

    hedgehog-unit = module: ''
      :load ${module}
      import ${module}
      import Hedgehog (check, property, test, withTests)
    '';

    tasty-tree = module: ''
      :load ${module}
      import ${module}
      import Test.Tasty (defaultMain)
    '';

    generic = module: ''
      :load ${module}
      import ${module}
    '';
  };

  builtinTestRunners = {
    hedgehog-property = name: "check ${name}";
    hedgehog-unit = name: "(check . withTests 1 . property . test) ${name}";
    tasty-tree = name: "defaultMain ${name}";
    generic = id;
  };

in rec {
  args = {
    basic = prelude:
    basicArgs ++ extraArgs ++ optional prelude "-XNoImplicitPrelude";

    command =
      commandArgs;

    preprocessor =
      ["-F" "-pgmF" ./preprocessor.bash] ++ preproc_options_ghc;
  };

  scripts = builtinTestScripts // testScripts;
  runners = builtinTestRunners // testRunners;

  script = key: scripts.${key} or scripts.generic;
  runner = key: runners.${key} or runners.generic;

  ghciScript = cwd: prelude: script:
  let
    scriptText = if builtins.isAttrs script then script.script else script;
  in
    cwdScript cwd + optionalString prelude preludeScript + scriptText;

  command = {
    packages,
    script,
    search,
    prelude ? true,
    cwd ? null,
  }:
    let
      # cwd = if builtins.isAttrs script && script ? cwd then script.cwd else null;
      basic = toString (args.basic prelude);
      command = toString args.command;
      preproc = toString args.preprocessor;
      searchP = searchPaths ((map libDir (builtins.attrValues packages)) ++ search);
      scriptFile = pkgs.writeText "ghci-script" (ghciScript cwd prelude script);
    in
    "ghci ${basic} ${command} ${preproc} ${searchP} -ghci-script ${scriptFile}";
}
