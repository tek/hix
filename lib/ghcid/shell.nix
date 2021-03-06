{ lib, config, command, withModules, }:
with builtins;
with lib;
let
  pkgs = config.internal.basicPkgs;

  vanillaGhc = config.devGhc.vanillaGhc;

  mainPackageNames = attrNames config.packages;

  getScript = c: key: c.ghci.scripts.${key} or c.ghci.scripts.generic;
  getRunner = c: key: c.ghci.runners.${key} or c.ghci.runners.generic;

  withShellConfig = overrides:
  withModules config [{ ghcid.shellConfig = overrides; }];

in rec {

  shellFor = {
    shellConfig,
    packageNames,
    hook ? "",
  }:
  let
    isNotTarget = p: !(p ? pname && elem p.pname packageNames);
    bInputs = p: p.buildInputs ++ p.propagatedBuildInputs;
    targetDeps = g: builtins.filter isNotTarget (concatMap bInputs (map (p: g.${p}) packageNames));
    hsPkgs = g: targetDeps g ++ shellConfig.haskellPackages g;
    devInputs = [
      (config.devGhc.ghc.ghcWithPackages hsPkgs)
      vanillaGhc.ghcid
      vanillaGhc.cabal-install
      config.shell.hls.package
    ];
    args = {
      name = "ghci-shell";
      buildInputs = devInputs ++ shellConfig.buildInputs;
      shellHook = hook;
    };
  in
    pkgs.stdenv.mkDerivation (args // shellConfig.env);

  runShell = name: {
    script,
    test,
    shellConfig,
    cwd ? null,
  }:
  let
    cmd = command.shellCommand { inherit script test shellConfig cwd; };
    shell = shellFor {
      packageNames = mainPackageNames;
      hook = cmd.script;
      inherit shellConfig;
    };
  in cmd // shell;

  shellWith = { packageNames ? mainPackageNames, shellConfig ? {}, hook ? "" }:
  withShellConfig shellConfig (c: shellFor { inherit packageNames hook; inherit (c.ghcid) shellConfig; });

  shellAppCmd = name: { script, test, shellConfig ? {} }:
  withShellConfig shellConfig (c:
    let cmd = (command.shellCommand { inherit script test; inherit (c.ghcid) shellConfig; }).script;
    in pkgs.writeScript "shell-${name}" "nix develop -c ${cmd}"
  );

  app = name: conf: {
    type = "app";
    program = "${shellAppCmd name conf}";
  };

  run = {
    pkg,
    module,
    name,
    type,
    runner,
    shellConfig ? {},
  }@args:
  withModules config [
    { ghcid.testConfig = _: config.ghcid.shellConfig; }
    { ghcid.testConfig = _: { search = ["$PWD/${pkg}/${type}"]; }; }
    { ghcid.testConfig = _: shellConfig; }
  ] (c:
    runShell "run" {
      cwd = pkg;
      shellConfig = c.ghcid.testConfig { inherit pkg module name type runner; };
      script = getScript c runner module;
      test = getRunner c runner name;
    }
  );

}
