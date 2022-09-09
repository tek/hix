{ lib, config, ... }:
with lib;
with types;
let

  tools = import ./project-tools.nix { inherit lib config; };

  defaultMain = makeOverridable tools config.devGhc;

  outPackagesFor = packages: ghc:
  lib.genAttrs packages (n: ghc.${n} // { inherit ghc; });

  compatChecks =
  let
    prefixed = prf: lib.mapAttrs' (n: v: { name = "${prf}-${n}"; value = v; });
    compatCheck = ver: conf: prefixed conf.prefix (outPackagesFor config.internal.packageNames conf.ghc.ghc);
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

  mainPackagesBase = outPackagesFor (config.internal.packageNames ++ config.output.extraPackages) project.ghc;

  mainPackages = extraDrvs {
    base = mainPackagesBase;
    min = config.minDevGhc;
    inherit (config) packages;
    inherit (project) pkgs;
  };

  extraChecks = if config.compat.enable then compatChecks else {};

  customized = config.output.transform project config.outputs;

  amended = config.output.amend project customized;

in {
  options = {

    output = {
      systems = mkOption {
        type = listOf str;
        description = "The systems for which to create outputs.";
        default = ["x86_64-linux"];
      };

      overrideMain = mkOption {
        type = unspecified;
        default = id;
        description = ''
        A function that allows customization of the generated dev package set.
        '';
      };

      transform = mkOption {
        type = functionTo (functionTo unspecified);
        default = _: id;
        description = ''
        A function taking the dev project and the generated outputs and returning modified outputs.
        The return value is not merged with the original outputs.
        '';
      };

      amend = mkOption {
        type = functionTo (functionTo unspecified);
        default = _: id;
        description = ''
        A function taking the dev project and the generated outputs and returning additional outputs.
        The return value is merged with the original outputs.
        '';
      };

      extraPackages = mkOption {
        description = ''
        Names of packages that will be added to the flake outputs, despite not being declared in
        <literal>options.packages</literal>.
        This may be a simple Hackage package like <literal>aeson</literal> or a local package that is added in
        <literal>options.overrides</literal> due to the way its source is obtained.
        '';
        type = listOf str;
        default = [];
      };

      final = mkOption {
        type = unspecified;
      };

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

    output = {
      final = mkDefault (attrsets.recursiveUpdate customized amended);
    };

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
      };

      devShells.default = config.ghcid.shell;

      apps = let
        app = program: { type = "app"; inherit program; };
        ghcid = app "${(config.ghcid.test {}).testApp}";
      in config.ghcid.apps // config.hackage.output.apps // config.hpack.apps mainPackages // {
        inherit ghcid;
        hls = app "${config.shell.hls.app}";
        hpack = app "${config.hpack.script}";
        tags = app "${project.tags.app}";
      };

    };

  };
}
