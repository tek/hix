inputs:
with builtins;
let
  util = {
    pure = import ./pure.nix;
    cabal-dep = import ./cabal-dep.nix;
    ghcOverlay = import ./ghc-overlay.nix;
    ghcNixpkgs = import ./ghc-nixpkgs.nix;
    ghcOverrides = import ./ghc-overrides.nix;
    ghci = import ./ghci.nix;
    ghcid = import ./ghcid.nix;
    tags = import ./tags.nix inputs;
    cabal = import ./cabal.nix;
    hpack = import ./hpack.nix;
    obelisk = import ./obelisk inputs;
    obeliskOverrides = import ./obelisk/overrides.nix inputs;
  };

  singlePackageMain = packages:
  let names = attrNames packages;
  in
  if length names == 1
  then head names
  else builtins.abort "'main' must be specified for multi-package projects";

  mainCompiler = "ghc8107";

  haskell = {
    system ? currentSystem,
    compiler ? mainCompiler,
    overrides ? _: {},
    cabal2nixOptions ? "",
    profiling ? true,
    nixpkgs ? inputs.nixpkgs,
    nixpkgsFunc ? import nixpkgs,
    overlays ? [],
    base,
    packages,
    ...
  }: rec {
    inherit compiler packages base nixpkgs;
    overlay = util.ghcOverlay {
      inherit base overrides compiler cabal2nixOptions profiling packages;
    };
    pkgs = nixpkgsFunc {
      inherit system;
      overlays = [overlay] ++ overlays;
      config.allowUnfree = true;
    };
    ghc = pkgs.haskell.packages.${compiler};
    basicGhc = (import inputs."nixpkgs_${mainCompiler}" { inherit system; }).haskell.packages.${mainCompiler};
  };

  tools = haskell: args@{
    base,
    packages,
    main ? singlePackageMain packages,
    runConfig ? _: {},
    testConfig ? _: _: {},
    compiler ? mainCompiler,
    ...
  }:
  let
    tls = import ./tools.nix { inherit (haskell) pkgs; };
    relative = tls.relativePackages base packages;
    ghciDefaults = {
      inherit (haskell) pkgs base;
      basicArgs = [
        "-Werror"
        "-Wall"
        "-Wredundant-constraints"
        "-Wunused-type-patterns"
        "-Widentities"
      ] ++ (if match "ghc81.*" compiler != null then ["-Wunused-packages"] else []);
    };
    ghci = util.ghci (ghciDefaults // args.ghci or {});
    ghcidDefaults = {
      inherit inputs main base;
      packages = relative;
      ghci = ghci;
      inherit (haskell) pkgs ghc compiler;
      runConfig = runConfig haskell;
      testConfig = testConfig haskell;
    };
    ghcid = util.ghcid (ghcidDefaults // args.ghcid or {});
  in
    haskell // {
      inherit ghci ghcid;
      tags = util.tags {
        packages = relative;
        inherit (haskell) compiler pkgs ghc;
      };
      cabal = util.cabal { inherit packages; inherit (haskell) pkgs; };
      hpack = { verbose ? false }: util.hpack { inherit verbose; inherit (haskell) pkgs; ghc = haskell.basicGhc; };
    };

  project = args: tools (haskell args) args;

  systemHook = f: args: system:
  let
    basicPkgs = import inputs.nixpkgs { inherit system; };
    tools = import ./tools.nix { pkgs = basicPkgs; };
    extra = tools // { inherit system basicPkgs; };
  in
    f (args // extra);

  systems = f: args: inputs.flake-utils.lib.eachSystem ["x86_64-linux"] (systemHook f args);

  defaultOutputs = {
    project,
    mainPackages,
    extraChecks,
    main,
    versionFile ? null,
  }:
  let
    app = program: { type = "app"; inherit program; };
    ghcid-test = app "${project.ghcid.ghcidTest.testApp}";
  in {
    defaultPackage = mainPackages.${main};
    devShell = project.ghcid.shell;
    legacyPackages = {
      project = project;
      pkgs = project.pkgs;
      ghc = project.ghc;
      ghcid = project.ghcid;
      run = project.ghcid.run;
      shell = project.ghcid.shell;
      cabal = project.cabal;
      tags = project.tags.projectTags;
      hpack = project.hpack {};
    };
    packages = mainPackages // extraChecks;
    checks = mainPackages // extraChecks;
    apps = project.ghcid.shellApps // {
      inherit ghcid-test;
      hpack = app "${project.hpack {}}";
      hpack-verbose = app "${project.hpack { verbose = true; }}";
      tags = app "${project.tags.app}";
      candidates = app "${project.cabal.candidates { name = main; inherit versionFile; }}";
      release = app "${project.cabal.release { name = main; inherit versionFile; }}";
      docs = app "${project.cabal.docs { name = main; }}";
    };
    defaultApp = ghcid-test;
  };

  customizeOutputs = {
    project,
    outputs,
    transform ? _: outputs: outputs,
    modify ? _: _: {},
    ...
  }:
  let customized = transform project outputs;
  in project.pkgs.lib.attrsets.recursiveUpdate customized (modify project customized);

  outPackagesFor = project: packages: ghc:
  let
    inherit (project.pkgs.lib.attrsets) genAttrs;
  in genAttrs (attrNames packages) (n: ghc.${n} // { inherit ghc; });

  defaultCompatVersions = ["901" "8107" "884"];

  # test the project with fixed nixpkgs and ghc version, and minimal overrides, for compatibility
  compatChecks = {
    project,
    packages,
    compatOverrides ? {},
    compatVersions ? defaultCompatVersions,
  }: args:
  let
    compatFor = n: let c = compatOverrides.${n} or []; in if builtins.isList c then c else [c];
    overrides = ver:
      if builtins.isAttrs compatOverrides
      then (compatFor "all") ++ (compatFor "ghc${ver}")
      else compatOverrides;
    compatProject = ver: haskell (args // {
      overrides = overrides ver;
      compiler = "ghc${ver}";
      nixpkgs = inputs."nixpkgs_ghc${ver}";
    });
    prefixed = prf: project.pkgs.lib.attrsets.mapAttrs' (n: v: { name = "${prf}-${n}"; value = v; });
    compatCheck = ver: (prefixed "compat-${ver}" (outPackagesFor project packages (compatProject ver).ghc));
  in
    foldl' (z: v: z // compatCheck v) {} compatVersions;

  flakeOutputs = {
    system,
    project,
    packages,
    main ? singlePackageMain packages,
    compiler ? mainCompiler,
    compat ? true,
    compatOverrides ? {},
    compatVersions ? defaultCompatVersions,
    versionFile ? null,
    transform ? _: outputs: outputs,
    modify ? _: _: {},
    ...
  }@args:
  let
    mainPackages = outPackagesFor project packages project.ghc;
    extraChecks = if compat then compatChecks { inherit project packages compatOverrides compatVersions; } args else {};
    outputs = defaultOutputs { inherit project mainPackages extraChecks main versionFile; };
  in customizeOutputs (args // { inherit project outputs; });

  defaultMain = args: args.basicPkgs.lib.makeOverridable project args;

  flakeWith = create: {
    packages,
    overrideMain ? p: p,
    ...
  }@args:
  systems (args: create (args // { project = overrideMain (defaultMain args); })) args;

in {
  inherit util haskell tools projectWithSets project systems flakeOutputs;
  inherit (util.pure) noOverrides;
  inherit (util) obeliskOverrides;

  flake = flakeWith flakeOutputs;
}
