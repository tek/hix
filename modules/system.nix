{ lib, config, withModules, ... }:
with lib;
let
  tools = import ./project-tools.nix { inherit lib config; };

  defaultMain = makeOverridable tools config.devGhc;

  defaultOutputs = {
    project,
    mainPackages,
    extraChecks,
  }:
  let
    app = program: { type = "app"; inherit program; };
    ghcid = app "${(config.ghcid.test {}).testApp}";
  in {
    devShells.default = config.ghcid.shell;
    legacyPackages = {
      inherit project config;
      inherit (project) pkgs ghc cabal;
      ghcid = config.ghcid;
      run = config.ghcid.run;
      shell = config.ghcid.shell;
      tags = project.tags.projectTags;
      hpack = project.hpack {};
    };
    packages = {
      min = mainPackages.${config.main}.min;
      default = mainPackages.${config.main};
    } // mainPackages // extraChecks;
    checks = mainPackages // extraChecks;
    apps = config.ghcid.apps // config.hackage.output.apps // config.hpack.apps mainPackages // {
      inherit ghcid;
      hls = app "${config.shell.hls.app}";
      hpack = app "${project.hpack {}}";
      hpack-verbose = app "${project.hpack { verbose = true; }}";
      tags = app "${project.tags.app}";
    };
  };

  customizeOutputs = outputs:
  let customized = config.output.transform defaultMain outputs;
  in attrsets.recursiveUpdate customized (config.output.amend defaultMain customized);

  outPackagesFor = packages: ghc:
  lib.genAttrs packages (n: ghc.${n} // { inherit ghc; });

  compatChecks =
  let
    prefixed = prf: lib.mapAttrs' (n: v: { name = "${prf}-${n}"; value = v; });
    compatCheck = ver: conf: prefixed conf.prefix (outPackagesFor (attrNames config.packages) conf.ghc.ghc);
  in
    foldl (z: v: z // v) {} (mapAttrsToList compatCheck config.compat.projects);

  releaseDrv = pkgs: drv:
  let
    hsLib = pkgs.haskell.lib;
  in
    import ../lib/release-derivation.nix { inherit lib hsLib; } drv;

  extraDrvs = { pkgs, base, min, packages }:
  let
    extra = n: _: base.${n} // { min = min.ghc.${n}; release = releaseDrv pkgs base.${n}; };
  in
    base // lib.mapAttrs extra packages;

  systemOutputs =
  let
    project = config.output.overrideMain defaultMain;
    mainPackagesBase = outPackagesFor (attrNames config.packages ++ config.output.extraPackages) project.ghc;
    mainPackages = extraDrvs {
      base = mainPackagesBase;
      min = config.minDevGhc;
      inherit (config) packages;
      inherit (project) pkgs;
    };
    extraChecks = if config.compat.enable then compatChecks else {};
    outputs = defaultOutputs { inherit project mainPackages extraChecks; };
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
