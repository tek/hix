{ lib, config, withModules, ... }:
with lib;
let
  tools = import ./project-tools.nix { inherit lib config; };

  defaultMain = makeOverridable tools config.devGhc;

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

  project = config.output.overrideMain defaultMain;

  mainPackagesBase = outPackagesFor (attrNames config.packages ++ config.output.extraPackages) project.ghc;

  mainPackages = extraDrvs {
    base = mainPackagesBase;
    min = config.minDevGhc;
    inherit (config) packages;
    inherit (project) pkgs;
  };

  extraChecks = if config.compat.enable then compatChecks else {};

  customized = config.output.transform defaultMain config.outputs;

  amended = config.output.amend defaultMain customized;

in {
  options = with types; {

    system = mkOption {
      type = str;
    };

    systemOutputs = mkOption {
      type = unspecified;
    };

    outputs = {

      packages = mkOption {
        type = attrsOf package;
        description = "The flake output attribute <literal>packages</literal>.";
      };

      checks = mkOption {
        type = attrsOf package;
        description = "The flake output attribute <literal>checks</literal>.";
      };

      legacyPackages = mkOption {
        type = attrsOf unspecified;
        description = "The flake output attribute <literal>legacyPackages</literal>.";
      };

      devShells = mkOption {
        type = attrsOf package;
        description = "The flake output attribute <literal>devShells</literal>.";
      };

      apps = mkOption {
        type = attrsOf unspecified;
        description = "The flake output attribute <literal>apps</literal>.";
      };

    };

  };

  config = {

    outputs = {

      packages = {
        min = mainPackages.${config.main}.min;
        default = mainPackages.${config.main};
      } // mainPackages // extraChecks;

      checks = mainPackages // extraChecks;

      legacyPackages = {
        inherit project config;
        inherit (project) pkgs ghc;
        ghcid = config.ghcid;
        run = config.ghcid.run;
        shell = config.ghcid.shell;
        tags = project.tags.projectTags;
        hpack = project.hpack {};
      };

      devShells.default = config.ghcid.shell;

      apps = let
        app = program: { type = "app"; inherit program; };
        ghcid = app "${(config.ghcid.test {}).testApp}";
      in config.ghcid.apps // config.hackage.output.apps // config.hpack.apps mainPackages // {
        inherit ghcid;
        hls = app "${config.shell.hls.app}";
        hpack = app "${project.hpack {}}";
        hpack-verbose = app "${project.hpack { verbose = true; }}";
        tags = app "${project.tags.app}";
      };

    };

    systemOutputs = mkDefault (attrsets.recursiveUpdate customized amended);

  };
}
