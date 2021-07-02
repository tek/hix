{
  inputs,
  pkgs,
  packages,
  main,
  ghci,
  ghc,
  compiler,
  base,
  commands ? _: {},
  prelude ? true,
  runConfig ? {},
  easy-hls ? true,
  ...
}:
with pkgs.lib;
with pkgs.lib.lists;
with pkgs.lib.debug;
let
  inherit (builtins) attrNames elem;
  inherit (pkgs) system;
  inherit (pkgs.haskell.lib) disableCabalFlag overrideCabal;

  vanillaGhc = (import inputs.nixpkgs { inherit system; }).haskell.packages.${compiler};
  cmds = commands { inherit pkgs ghc; };
  tools = import ./tools.nix { inherit pkgs; };
  cabal-dep = import ./cabal-dep.nix { inherit pkgs; profiling = true; };

  configEmpty = {
    env = {};
    extraShellInputs = [];
    extraShellPackages = _: [];
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
    concat = attr: l.${attr} ++ r.${attr};
  in {
    env = l.env // r.env;
    extraSearch = concat "extraSearch";
    extraRestarts = concat "extraRestarts";
    extraShellInputs = concat "extraShellInputs";
    extraShellPackages = g: l.extraShellPackages g ++ r.extraShellPackages g;
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

  pkgRestarts = attrsets.mapAttrsToList (n: pkg: restart "$PWD/${pkg}/${n}.cabal");

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

  hlsGhc = cabal-dep.override vanillaGhc ({ jailbreak, source, hackage, minimal, ... }: 
  let
    hls = p: source.sub inputs.hls p;
    plug = n: hls "plugins/hls-${n}-plugin";
  in {
    blaze-textual = source.root inputs.blaze-textual;
    cryptohash-md5 = jailbreak;
    cryptohash-sha1 = jailbreak;
    czipwith = source.root inputs.czipwith;
    dependent-sum-template = minimal (source.sub inputs.dependent-sum "dependent-sum-template");
    ghc-api-compat = source.root inputs.ghc-api-compat;
    ghc-lib-parser = hackage "9.0.1.20210324" "0kf45jnp62lwfv585c5rfpxw7ywbz92ivxx7h53nxqa1dw5di7qp";
    ghcide = hackage "1.4.0.3" "17hf0vqpv3imhfdhf7xwfknvl1dwadx6x7i9d36xb41va6rpf1yr";
    haskell-language-server = minimal (source.root inputs.hls);
    hie-bios = minimal (source.root inputs.hie-bios);
    hie-compat = hackage "0.2.0.0" "1ygyndd02ri2bqv7v08dx3g9cdliqgyhd103cfgbd56qp48nckdn";
    hiedb = hackage "0.4.0.0" "13jz8c46zfpf54ya2wsv4akhn0wcfc6qjazqsjfir5gpvsi7v8xr";
    hls-module-name-plugin = hackage "1.0.0.0" "1zpa7ahxyb7j3ssc93q02idp0bzpm4bpzhj8pyjb47yaavqhlz64";
    hls-floskell-plugin = hackage "1.0.0.0" "04yqz8cd8r32nl3c5vni8yp0m6w1zjrxifvj55ag0x775fyq12s5";
    hls-explicit-imports-plugin = hackage "1.0.0.3" "0i9gk835vr2bfa17qzfxk51js3dgbz17179wp5mk0jsih12hz4q1";
    hls-ormolu-plugin = hackage "1.0.0.0" "1y1fq6yvcqcymc4xzxjscpa0d225zsvx9wqsajx9pcwgz04qbgy5";
    hls-fourmolu-plugin = hackage "1.0.0.1" "1klk6qlr5x1rs3sy6sqbnjir0slwi7596j69sd8db4gavrl2d4yi";
    hls-pragmas-plugin = minimal (plug "pragmas");
    hls-refine-imports-plugin = plug "refine-imports";
    hls-haddock-comments-plugin = hackage "1.0.0.2" "03nxlcmmsyixm7277pci645zp95nlphwrhf5cra6qv647b7j4yzb";
    hls-graph = hls "hls-graph";
    hls-plugin-api = hackage "1.1.0.2" "16g7m2nzdr4zlsc49y8zgswgrjkg3cgdlbrm5xdhq9v2q1s3wxgg";
    hls-test-utils = hls "hls-test-utils";
    hslogger = jailbreak;
    lsp = source.sub inputs.lsp "lsp";
    lsp-test = minimal (source.sub inputs.lsp "lsp-test");
    lsp-types = minimal (source.sub inputs.lsp "lsp-types");
    mono-traversable = minimal;
    semialign = hackage "1.2" "1crxnbz66k1qw8yrial8qa50z7q538q618ml1n4c5yghvpdgs1kx";
    th-extras = source.root inputs.th-extras;
  });

  disabledPlugins =
    ["brittany" "floskell" "stylishHaskell" "fourmolu" "ormolu" "retrie" "hlint"] ++
    (if compiler == "ghc901" then ["eval" "class" "splice" "tactic" "refineImports"] else []);

  disabledDepsNames =
    ["brittany" "floskell" "stylish-haskell" "fourmolu" "ormolu" "retrie" "hlint"] ++
    (if compiler == "ghc901" then ["eval" "class" "splice" "tactics" "refine-imports"] else []);

  disabledDeps =
    map (a: "hls-${a}-plugin") disabledDepsNames;

  withoutPlugins =
    builtins.foldl' disableCabalFlag hlsGhc.haskell-language-server disabledPlugins;

  removeDeps = old: {
    executableHaskellDepends =
      filter (d: d == null || all (a: d.pname or "" != a) disabledDeps) old.executableHaskellDepends;
  };

  hlsCustom = overrideCabal withoutPlugins (old: old // removeDeps old);

  hls =
    if easy-hls then inputs.easy-hls.defaultPackage.${system} else hlsCustom;

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
  }:
  let
    conf = fullConfig config;
    mainCommand = ghci.command {
      packages = packages;
      inherit script prelude;
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
  }:
  ghciShellFor "run" {
    config = mergeConfig config { extraSearch = ["$PWD/${pkg}/${type}"]; };
    script = ghci.scripts.run pkg module runner;
    test = ghci.tests.test name runner;
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
  cmd = ghcidCmd;
  shell = shellWith {};
  commands = cmds;
  shellApps = builtins.mapAttrs shellApp cmds;
}
