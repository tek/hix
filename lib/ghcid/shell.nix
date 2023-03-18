{ lib, config, command, util, }:
with builtins;
with lib;
let
  pkgs = config.internal.basicPkgs;

  vanillaGhc = config.devGhc.vanillaGhc;

  mainPackageNames = config.internal.packageNames;

  getScript = c: key: c.ghci.scripts.${key} or c.ghci.scripts.generic;
  getRunner = c: key: c.ghci.runners.${key} or c.ghci.runners.generic;

  withShellConfig = overrides:
  util.withModules config [{ ghcid.shellConfig = overrides; }];

  wantGhcid = config.shell.ghcid.enable;
  vanillaGhcid = config.shell.ghcid.vanilla;

in rec {

  shellFor = {
    shellConfig ? {},
    packageNames ? mainPackageNames,
    hook ? "",
  }:
  let
    isNotTarget = p: !(p ? pname && elem p.pname packageNames);
    bInputs = p: p.buildInputs ++ p.propagatedBuildInputs;
    targetDeps = g: builtins.filter isNotTarget (concatMap bInputs (map (p: g.${p}) packageNames));
    hsPkgs = g:
      targetDeps g ++ shellConfig.haskellPackages g;
    devInputs = [
      (config.devGhc.ghc.ghcWithPackages hsPkgs)
      config.shell.hls.package
    ] ++ optional wantGhcid (if vanillaGhcid then config.pkgs.haskell.lib.dontCheck vanillaGhc.ghcid else config.devGhc.ghc.ghcid);
    args = {
      name = "ghci-shell";
      buildInputs = devInputs ++ shellConfig.buildInputs;
      shellHook = hook;
    };
  in
    pkgs.stdenv.mkDerivation (args // shellConfig.env);

  runEnv = name: {
    script,
    test,
    shellConfig,
    cwd ? null,
  }: let
    shellCommand = command.shellCommand { inherit script test shellConfig cwd; };
  in {
    inherit shellCommand;
    command = shellCommand.script;
    shell = shellFor {
      packageNames = mainPackageNames;
      inherit shellConfig;
    };
  };

  runShell = name: args: let
    sh = runEnv name args;
  in sh.shellCommand // sh.shell.overrideAttrs (_: { shellHook = sh.command; });

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

  test = {
    pkg ? config.main,
    module ? "Main",
    name ? "main",
    type ? "test",
    runner ? "generic",
    shellConfig ? {},
  }:
  util.withModules config [
    { ghcid.testConfig = _: config.ghcid.shellConfig; }
    { ghcid.testConfig = _: { search = ["$PWD/${pkg}/${type}"]; }; }
    { ghcid.testConfig = _: shellConfig; }
  ] (c: runEnv "test" {
      cwd = pkg;
      shellConfig = c.ghcid.testConfig { inherit pkg module name type runner; };
      script = getScript c runner module;
      test = getRunner c runner name;
    }
  );

  run = args: let
    env = test args;
  in env.shellCommand // env.shell.overrideAttrs (_: { shellHook = env.command; });

}
