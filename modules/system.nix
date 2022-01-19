{ lib, config, withModules, ... }:
with lib;
let
  tools = import ./project-tools.nix { inherit lib config; };

  defaultMain = makeOverridable tools config.devGhc;

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
    ghcid = app "${(config.ghcid.test {}).testApp}";
  in {
    defaultPackage = mainPackages.${main};
    devShell = config.ghcid.shell;
    legacyPackages = {
      inherit project config;
      inherit (project) pkgs ghc cabal;
      ghcid = config.ghcid;
      run = config.ghcid.run;
      shell = config.ghcid.shell;
      tags = project.tags.projectTags;
      hpack = project.hpack {};
    };
    packages = { min = mainPackages.${main}.min; } // mainPackages // extraChecks;
    checks = mainPackages // extraChecks;
    apps = config.ghcid.apps // {
      inherit ghcid;
      # TODO deprecate
      ghcid-test = ghcid;
      hls = app "${config.ghcid.hlsApp}";
      hpack = app "${project.hpack {}}";
      hpack-verbose = app "${project.hpack { verbose = true; }}";
      tags = app "${project.tags.app}";
      candidates = app "${project.cabal.candidates { name = main; inherit versionFile; }}";
      release = app "${project.cabal.release { name = main; inherit versionFile; }}";
      docs = app "${project.cabal.docs { name = main; }}";
    };
    defaultApp = ghcid;
  };

  customizeOutputs = outputs:
  let customized = config.output.transform defaultMain outputs;
  in attrsets.recursiveUpdate customized (config.output.amend defaultMain customized);

  outPackagesFor = packages: ghc:
  lib.genAttrs (attrNames packages) (n: ghc.${n} // { inherit ghc; });

  compatChecks =
  let
    prefixed = prf: lib.mapAttrs' (n: v: { name = "${prf}-${n}"; value = v; });
    compatCheck = ver: conf: prefixed conf.prefix (outPackagesFor config.packages conf.ghc.ghc);
  in
    foldl (z: v: z // v) {} (mapAttrsToList compatCheck config.compat.projects);

  addMinPackages = { base, min, packages }:
  base // lib.mapAttrs (n: _: base.${n} // { min = min.ghc.${n}; }) packages;

  systemOutputs =
  let
    project = config.output.overrideMain defaultMain;
    mainPackagesBase = outPackagesFor config.packages project.ghc;
    mainPackages = addMinPackages { base = mainPackagesBase; min = config.minDevGhc; inherit (config) packages; };
    extraChecks = if config.compat.enable then compatChecks else {};
    outputs = defaultOutputs { inherit (config) main; inherit project mainPackages extraChecks; };
  in customizeOutputs outputs;

in {
  options = {
    system = mkOption {
      type = types.str;
    };
    systemOutputs = mkOption {
      type = types.unspecified;
    };
  };

  config.systemOutputs = mkDefault systemOutputs;
}
