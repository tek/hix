inputs:
with builtins;
let

  inherit (inputs.nixpkgs) lib;

  util = {
    pure = import ./pure.nix;
    cabal-dep = import ./cabal-dep.nix;
    spec = import ./deps/spec.nix { inherit lib; };
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

  tls = import ./tools.nix { inherit lib; };

  # Import nixpkgs, adding an overlay that contains `cabal2nix` derivations for local packages and the specified
  # dependency overrides.
  haskell = {
    system ? currentSystem,
    compiler ? mainCompiler,
    overrides ? [],
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
      inherit base compiler cabal2nixOptions profiling packages overrides;
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
    hpackDir ? "ops/hpack",
    hpackShared ? "shared",
    ...
  }:
  let
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
      hpack = { verbose ? false }: util.hpack {
        inherit verbose hpackDir hpackShared;
        inherit (haskell) pkgs;
        ghc = haskell.basicGhc;
        paths = relative;
      };
    };

  project = args@{ overrides ? {}, ... }:
  let
    os =
      tls.overridesFor overrides "all" ++
      tls.overridesFor overrides "dev";
    a =
      args // { overrides = os; };
  in tools (haskell a) a;

  systems = f: args@{ systems ? ["x86_64-linux"], ... }:
  inputs.flake-utils.lib.eachSystem systems (system: f (args // { inherit system; }));

  # /Note/: Overrides are not normalized.
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
      hls = app "${project.ghcid.hlsApp}";
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

  # Derivations for local packages with fixed nixpkgs and ghc version, and minimal overrides, for compatibility checks.
  # /Note/: Overrides must be normalized;
  compatChecks = {
    project,
    packages,
    overrides ? {},
    compatVersions ? defaultCompatVersions,
  }: args:
  let
    compatOverrides = ver:
    tls.overridesFor overrides "all" ++
    tls.overridesFor overrides "compat" ++
    tls.overridesFor overrides "ghc${ver}";

    compatProject = ver: haskell (args // {
      overrides = compatOverrides ver;
      compiler = "ghc${ver}";
      nixpkgs = inputs."nixpkgs_ghc${ver}";
    });

    prefixed = prf: project.pkgs.lib.attrsets.mapAttrs' (n: v: { name = "${prf}-${n}"; value = v; });

    compatCheck = ver: (prefixed "compat-${ver}" (outPackagesFor project packages (compatProject ver).ghc));
  in
    foldl' (z: v: z // compatCheck v) {} compatVersions;

  # /Note/: Overrides are not normalized.
  flakeOutputs = {
    system,
    project,
    packages,
    main ? singlePackageMain packages,
    compiler ? mainCompiler,
    compat ? true,
    overrides ? {},
    compatVersions ? defaultCompatVersions,
    versionFile ? null,
    transform ? _: outputs: outputs,
    modify ? _: _: {},
    ...
  }@args:
  let
    mainPackages = outPackagesFor project packages project.ghc;
    extraChecks = if compat then compatChecks { inherit project packages overrides compatVersions; } args else {};
    outputs = defaultOutputs { inherit project mainPackages extraChecks main versionFile; };
  in customizeOutputs (args // { inherit project outputs; });

  defaultMain = args: lib.makeOverridable project args;

  flakeWith = create: {
    overrideMain ? p: p,
    overrides ? {},
    deps ? [],
    ...
  }@args:
  let
    os = tls.normalizeOverrides overrides deps;
    f = a: create (a // { project = overrideMain (defaultMain a); });
  in systems f (args // { overrides = os; }) // { overrides = os; };

  tests = system: import ./test { pkgs = import inputs.nixpkgs { inherit system; }; };

  systemOutputs = inputs.flake-utils.lib.eachSystem ["x86_64-linux"] (system: {
    apps = {
      test = {
        type = "app";
        program = "${(tests system).main}";
      };
    };
  });

in systemOutputs // {
  inherit util haskell tools projectWithSets project systems flakeOutputs;
  inherit (util) obeliskOverrides;

  flake = flakeWith flakeOutputs;
}
