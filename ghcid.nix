{
  inputs,
  pkgs,
  packages,
  main,
  ghci,
  ghc,
  compiler,
  base,
  nixpkgs,
  commands ? _: {},
  prelude ? true,
  runConfig ? {},
  testConfig ? _: {},
  easy-hls ? false,
  ghc9 ? false,
  ...
}:
with pkgs.lib;
let
  inherit (builtins) attrNames elem;
  inherit (pkgs) system;
  inherit (pkgs.haskell.lib) disableCabalFlag overrideCabal;

  vanillaGhc = (import inputs.nixpkgs { inherit system; }).haskell.packages.${compiler};
  cmds = commands { inherit pkgs ghc; };
  tools = import ./tools.nix { inherit pkgs; };
  hls = import ./hls.nix { inherit vanillaGhc easy-hls inputs compiler pkgs system ghc9; };
  vms = import ./vm.nix { inherit nixpkgs pkgs; };

  configEmpty = {
    env = {};
    extraShellInputs = [];
    extraShellPackages = _: [];
    extraSearch = [];
    extraRestarts = [];
    preCommand = [];
    preStartCommand = [];
    exitCommand = [];
    vm = null;
  };

  unlines = concatStringsSep "\n";

  mergeConfig = left: right:
  let
    l = configEmpty // left;
    r = configEmpty // right;
    concat = attr: toList l.${attr} ++ toList r.${attr};
  in {
    env = l.env // r.env;
    extraSearch = concat "extraSearch";
    extraRestarts = concat "extraRestarts";
    extraShellInputs = concat "extraShellInputs";
    extraShellPackages = g: l.extraShellPackages g ++ r.extraShellPackages g;
    preCommand = concat "preCommand";
    preStartCommand = concat "preStartCommand";
    exitCommand = concat "exitCommand";
    vm = if isNull r.vm then l.vm else r.vm;
  };

  fullConfig = user: mergeConfig (mergeConfig configEmpty runConfig) user;

  restart = f: ''--restart="${f}"'';

  pkgRestarts = attrsets.mapAttrsToList (n: pkg: restart "$PWD/${pkg}/${n}.cabal");

  ghcidCmd =
    command: test: extraRestarts:
    let
      restarts = (pkgRestarts packages) ++ (map restart extraRestarts);
    in
      ''ghcid -W ${toString restarts} --command="${command}" --test='${test}' '';

  startVm = vm: if isNull vm then "" else vms.ensure vm;

  stopVm = vm: if isNull vm then "" else vms.kill vm;

  ghcidCmdFile = {
    command,
    test,
    extraRestarts,
    preStartCommand,
    exitCommand,
    vm,
    ...
  }:
  let
    vmData = if isNull vm then null else
    let
      type = vm.type or "create";
      vmCreate = vm.create or vms.${type};
    in vmCreate vm;

  in pkgs.writeScript "ghcid-cmd" ''
    #!${pkgs.zsh}/bin/zsh
    quitting=0
    quit() {
      if [[ $quitting == 0 ]]
      then
        quitting=1
        print ">>> quitting due to signal $1"
        ${stopVm vmData}
        ${unlines exitCommand}
        # kill zombie GHCs
        ${pkgs.procps}/bin/pkill -9 -x -P 1 ghc
      fi
      return 1
    }
    TRAPINT() { quit $* }
    TRAPTERM() { quit $* }
    TRAPKILL() { quit $* }
    TRAPEXIT() { quit $* }
    ${unlines preStartCommand}
    ${startVm vmData}
    ${ghcidCmd command test extraRestarts}
  '';

  shellFor = {
    packageNames,
    hook ? "",
    config ? {},
  }:
  let
    conf = fullConfig config;
    isNotTarget = p: !(p ? pname && elem p.pname packageNames);
    bInputs = p: p.buildInputs ++ p.propagatedBuildInputs;
    targetDeps = g: builtins.filter isNotTarget (concatMap bInputs (map (p: g.${p}) packageNames));
    hsPkgs = g: targetDeps g ++ conf.extraShellPackages g;
    devInputs = [
      (ghc.ghcWithPackages hsPkgs)
      vanillaGhc.ghcid
      vanillaGhc.cabal-install
      hls
    ];
    args = {
      name = "ghci-shell";
      buildInputs = devInputs ++ conf.extraShellInputs;
      shellHook = hook;
    };
  in
    pkgs.stdenv.mkDerivation (args // conf.env);

  ghcidShellCmd = {
    script,
    test,
    config ? {},
    cwd ? null,
  }:
  let
    conf = fullConfig config;
    mainCommand = ghci.command {
      packages = packages;
      inherit script prelude cwd;
      inherit (conf) extraSearch;
    };
    command = ''
      ${unlines conf.preCommand}
      ${mainCommand}
    '';
  in ghcidCmdFile (conf // { inherit command test; });

  ghciShellFor = name: {
    script,
    test,
    config ? {},
    cwd ? null,
  }:
  shellFor {
    packageNames = attrNames packages;
    hook = ghcidShellCmd { inherit script test config cwd; };
    inherit config;
  };

  shells = builtins.mapAttrs ghciShellFor cmds;

  shellWith = args: shellFor ({ packageNames = attrNames packages; } // args);

  ghcidTestWith = args: import ./ghcid-test.nix ({ inherit pkgs; } // args);

  ghcidTest = ghcidTestWith {};

  shellAppCmd = name: config: pkgs.writeScript "shell-${name}" "nix develop -c ${ghcidShellCmd config}";

  shellApp = name: config: {
    type = "app";
    program = "${shellAppCmd name config}";
  };

  run = {
    pkg,
    module,
    name,
    type,
    runner,
    config ? {},
  }@args:
  ghciShellFor "run" {
    cwd = pkg;
    config = mergeConfig (mergeConfig config { extraSearch = ["$PWD/${pkg}/${type}"]; }) (testConfig args);
    script = ghci.script runner module;
    test = ghci.runner runner name;
  };

in shells // {
  inherit shells shellFor shellWith ghcidCmdFile ghciShellFor haskell-language-server ghcidTestWith ghcidTest;
  run = makeOverridable run {
    pkg = main;
    module = "Main";
    name = "main";
    type = "test";
    runner = "generic";
  };
  hls = haskell-language-server;
  hlsApp = pkgs.writeScript "hls" "nix develop -c haskell-language-server";
  cmd = ghcidCmd;
  shell = shellWith {};
  commands = cmds;
  shellApps = builtins.mapAttrs shellApp cmds;
}
