inputs:
with builtins;
let

  inherit (inputs.nixpkgs) lib;

  util = {
    lib = import ./lib.nix { inherit lib; };
    spec = import ./deps/spec.nix { inherit lib; };
    ghcOverlay = import ./ghc-overlay.nix;
    ghcOverrides = import ./overrides.nix;
    ghci = import ./ghci.nix;
    ghcid = import ./ghcid.nix;
    tags = import ./tags.nix inputs;
    cabal = import ./cabal.nix;
    hpack = import ./hpack.nix;
    obelisk = import ./obelisk inputs;
    obeliskOverrides = import ./obelisk/overrides.nix inputs;
    vm = import ./vm;
  };

  hlib = util.lib;

  singlePackageMain = packages:
  let names = attrNames packages;
  in
  if length names == 1
  then head names
  else throw "'main' must be specified for multi-package projects";

  mainCompiler = "ghc8107";

  projectOverrides = {
    base,
    packages,
    deps ? [],
    overrides ? [],
    profiling ? true,
    localPackage ? _: lib.id,
  }:
  let
    local = import ./deps/local.nix { inherit lib base packages localPackage; };
    localMin = import ./deps/local.nix {
      inherit lib base packages;
      localPackage = { fast, ... }: p: fast p;
    };
    withDeps = util.lib.normalizeOverrides overrides deps;
  in withDeps // { all = (withDeps.local or []) ++ withDeps.all; local = [local]; localMin = [localMin]; };

  # Import nixpkgs, adding an overlay that contains `cabal2nix` derivations for local packages and the specified
  # dependency overrides.
  haskell = {
    base,
    packages,
    system ? currentSystem,
    compiler ? mainCompiler,
    overrides ? {},
    overrideKeys ? ["local" "all" compiler "dev"],
    profiling ? true,
    nixpkgs ? inputs.nixpkgs,
    nixpkgsFunc ? import nixpkgs,
    overlays ? [],
    ...
  }:
  let
    overlay = util.ghcOverlay { inherit compiler profiling overrides overrideKeys; };
  in rec {
    inherit compiler packages base nixpkgs overlay;
    pkgs = nixpkgsFunc {
      inherit system;
      overlays = [overlay] ++ overlays;
      config.allowUnfree = true;
    };
    ghc = pkgs.hixPackages;
    basicGhc = (import inputs."nixpkgs_${mainCompiler}" { inherit system; }).haskell.packages.${mainCompiler};
  };

  tools = haskell: args@{
    base,
    packages,
    main ? singlePackageMain packages,
    shellConfig ? {},
    testConfig ? {},
    compiler ? mainCompiler,
    hpackDir ? "ops/hpack",
    hpackShared ? "shared",
    ...
  }:
  let
    relative = util.lib.relativePackages base packages;
    ghciDefaults = {
      inherit (haskell) pkgs base;
      basicArgs = [
        "-Werror"
        "-Wall"
        "-Wredundant-constraints"
        "-Wunused-type-patterns"
        "-Widentities"
      ] ++ (if match "ghc(81|9).*" compiler != null then ["-Wunused-packages"] else []);
    };
    ghci = util.ghci (ghciDefaults // args.ghci or {});
    ghcidDefaults = {
      inherit inputs main base;
      packages = relative;
      ghci = ghci;
      inherit (haskell) pkgs ghc compiler nixpkgs;
      shellConfig = hlib.asFunction shellConfig haskell;
      testConfig = hlib.asFunction testConfig haskell;
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

  project = args: tools (haskell args) args;

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
    packages = { min = mainPackages.${main}.min; } // mainPackages // extraChecks;
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

  outPackagesFor = packages: ghc:
  let
  in lib.genAttrs (attrNames packages) (n: ghc.${n} // { inherit ghc; });

  defaultCompatVersions = ["902" "8107" "884"];

  # Derivations for local packages with fixed nixpkgs and ghc version, and minimal overrides, for compatibility checks.
  # /Note/: Overrides must be normalized;
  compatChecks = {
    packages,
    overrides ? {},
    compatVersions ? defaultCompatVersions,
  }: args:
  let
    compiler = "ghc${ver}";
    compatProject = ver: haskell (args // {
      inherit compiler;
      nixpkgs = inputs."nixpkgs_ghc${ver}";
      overrideKeys = ["local" "all" "compat" compiler];
    });

    prefixed = prf: lib.mapAttrs' (n: v: { name = "${prf}-${n}"; value = v; });

    compatCheck = ver: (prefixed "compat-${ver}" (outPackagesFor packages (compatProject ver).ghc));
  in
    foldl' (z: v: z // compatCheck v) {} compatVersions;

  addMinPackages = { base, min, packages }:
  base // lib.mapAttrs (n: _: base.${n} // { min = min.ghc.${n}; }) packages;

  # /Note/: Overrides are not normalized.
  systemOutputs = {
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
    mainPackagesBase = outPackagesFor packages project.ghc;
    minPackages = haskell (args // { overrrideKeys = ["localMin" "all" compiler "dev"]; } );
    mainPackages = addMinPackages { base = mainPackagesBase; min = minPackages; inherit packages; };
    extraChecks = if compat then compatChecks { inherit packages overrides compatVersions; } args else {};
    outputs = defaultOutputs { inherit project mainPackages extraChecks main versionFile; };
  in customizeOutputs (args // { inherit project outputs; });

  defaultMain = args: lib.makeOverridable project args;

  flakeWith = create: {
    base,
    packages,
    deps ? [],
    overrides ? {},
    profiling ? true,
    localPackage ? _: lib.id,
    overrideMain ? p: p,
    ...
  }@args:
  let
    fullOverrides = projectOverrides {
      inherit base packages deps overrides profiling localPackage;
    };
    f = a: create (a // { project = overrideMain (defaultMain a); });
    outputs = systems f (args // { overrides = fullOverrides; });
  in outputs // { overrides = fullOverrides; };

  tests = system: import ./test { pkgs = import inputs.nixpkgs { inherit system; }; };

  localOutputs = inputs.flake-utils.lib.eachSystem ["x86_64-linux"] (system: {
    apps = {
      test = {
        type = "app";
        program = "${(tests system).main}";
      };
    };
  });

in localOutputs // {
  inherit util projectOverrides haskell tools project systems systemOutputs;
  inherit (util) obeliskOverrides lib;

  flake = flakeWith systemOutputs;
}
