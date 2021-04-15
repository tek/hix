{
  inputs,
  pkgs,
  packages,
  main,
  ghci,
  ghc,
  compiler,
  commands ? _: {},
  extraShellInputs ? [],
  extraShellPackages ? _: [],
  prelude ? true,
  runConfig ? {},
  ...
}:
with pkgs.lib.lists;
let
  version = ghc.ghc.version;
  lib = pkgs.lib;
  inherit (pkgs.haskell.lib) enableCabalFlag;
  inherit (builtins) attrNames elem;
  vanillaGhc = (import inputs.nixpkgs { inherit (pkgs) system; }).haskell.packages.${compiler};
  haskell-language-server = vanillaGhc.haskell-language-server;
  cmds = commands { inherit pkgs ghc; };

  configEmpty = {
    env = {};
    extraSearch = [];
    extraRestarts = [];
    preCommand = "";
    preStartCommand = "";
    exitCommand = "";
  };

  mergeConfig = left: right:
  let
    l = configEmpty // left;
    r = configEmpty // right;
  in {
    env = l.env // r.env;
    extraSearch = l.extraSearch ++ r.extraSearch;
    extraRestarts = l.extraRestarts ++ r.extraRestarts;
    preCommand = ''
      ${l.preCommand}
      ${r.preCommand}
    '';
    preStartCommand = ''
      ${l.preStartCommand}
      ${r.preStartCommand}
    '';
    exitCommand = ''
      ${l.exitCommand}
      ${r.exitCommand}
    '';
  };

  fullConfig = user: mergeConfig (mergeConfig configEmpty runConfig) user;

  restart = f: ''--restart="${f}"'';

  pkgRestarts = lib.attrsets.mapAttrsToList (n: pkg: restart "$PWD/${pkg}/${n}.cabal");

  ghcidCmd =
    command: test: extraRestarts:
    let
      restarts = (pkgRestarts packages) ++ (map restart extraRestarts);
    in
      ''ghcid -W ${toString restarts} --command="${command}" --test='${test}' '';

  ghcidCmdFile = {
    command,
    test,
    extraRestarts,
    preStartCommand,
    exitCommand,
    ...
  }:
  pkgs.writeScript "ghcid-cmd" ''
    ${preStartCommand}
    ${ghcidCmd command test extraRestarts}
    ${exitCommand}
  '';

  shellFor = {
    packageNames,
    hook ? "",
    env ? {},
  }:
  let
    isNotTarget = p: !(p ? pname && elem p.pname packageNames);
    inputs = p: p.buildInputs ++ p.propagatedBuildInputs;
    hsPkgs = g: builtins.filter isNotTarget (concatMap inputs (map (p: g.${p}) packageNames)) ++ extraShellPackages g;
    devInputs = [(ghc.ghcWithPackages hsPkgs) vanillaGhc.ghcid vanillaGhc.cabal-install haskell-language-server];
    args = {
      name = "ghci-shell";
      buildInputs = devInputs ++ extraShellInputs;
      shellHook = hook;
    };
  in
    pkgs.stdenv.mkDerivation (args // env);

  ghcidShellCmd = {
    script,
    test,
    config ? {},
  }:
  let
    conf = fullConfig config;
    mainCommand = ghci.command {
      inherit packages script prelude;
      inherit (conf) extraSearch;
    };
    command = ''
      ${conf.preCommand}
      ${mainCommand}
    '';
  in ghcidCmdFile (conf // { inherit command test; });

  ghciShellFor = name: {
    script,
    test,
    config ? {},
  }:
  shellFor {
    packageNames = attrNames packages;
    hook = ghcidShellCmd { inherit script test config; };
    env = config.env or {};
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
  }:
  ghciShellFor "run" {
    config = mergeConfig config { extraSearch = ["$PWD/${pkg}/${type}"]; };
    script = ghci.scripts.run pkg module runner;
    test = ghci.tests.test name runner;
  };

in shells // {
  inherit shells shellFor shellWith ghcidCmdFile ghciShellFor haskell-language-server ghcidTestWith ghcidTest;
  run = lib.makeOverridable run {
    pkg = main;
    module = "Main";
    name = "main";
    type = "test";
    runner = "generic";
  };
  hls = haskell-language-server;
  cmd = ghcidCmd;
  shell = shellWith {};
  commands = cmds;
  shellApps = builtins.mapAttrs shellApp cmds;
}
