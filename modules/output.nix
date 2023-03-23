{ lib, config, util, ... }:
with lib;
with types;
let

  project = config.devGhc;

  outPackagesFor = packages: ghc:
  genAttrs packages (n: ghc.${n} // { inherit ghc; });

  compatChecks = let
    prefixed = prf: mapAttrs' (n: v: { name = "${prf}-${n}"; value = v; });
    compatCheck = ver: conf:
    if conf.enable
    then prefixed conf.prefix (outPackagesFor config.internal.packageNames conf.ghc.ghc)
    else {};
  in
    foldl (z: v: z // v) {} (mapAttrsToList compatCheck config.compat.projects);

  releaseDrv =
    import ../lib/release-derivation.nix {
      inherit lib;
      hsLib = config.pkgs.haskell.lib;
    };

  base = outPackagesFor (config.internal.packageNames ++ config.output.extraPackages) project.ghc;

  main = let
    extra = n: base.${n} // { min = config.minDevGhc.ghc.${n}; release = releaseDrv base.${n}; };
  in
    base // genAttrs config.internal.packageNames extra;

  extraChecks = if config.compat.enable then compatChecks else {};

  transformed = config.output.transform main config.outputs;

  amended = config.output.amend main transformed;

  tags = import ../lib/tags.nix { inherit config; };

  showConfig = import ../lib/show-config.nix { inherit config lib util; };

  show-config = util.paramApp {
    name = "show-config";
    func = showConfig;
    params = ["path"];
  };

  ghcid-test =
    util.paramApp {
      name = "ghcid";
      func = config.ghcid.lib.shell.test;
      params = ["pkg" "module" "name" "type" "runner"];
      shellName = "ghcid-run";
    };

in {
  options = {

    output = {
      systems = mkOption {
        type = listOf str;
        description = mdDoc "The systems for which to create outputs.";
        default = ["x86_64-linux"];
      };

      transform = mkOption {
        type = functionTo (functionTo unspecified);
        default = _: id;
        description = mdDoc ''
        A function taking the dev project and the generated outputs and returning modified outputs.
        The return value is not merged with the original outputs.
        '';
      };

      amend = mkOption {
        type = functionTo (functionTo unspecified);
        default = _: _: {};
        description = mdDoc ''
        A function taking the dev project and the generated outputs and returning additional outputs.
        The return value is merged with the original outputs.
        '';
      };

      extraPackages = mkOption {
        description = mdDoc ''
        Names of packages that will be added to the flake outputs, despite not being declared in
        `options.packages`.
        This may be a simple Hackage package like `aeson` or a local package that is added in
        `options.overrides` due to the way its source is obtained.
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
        type = lazyAttrsOf package;
        description = mdDoc "The flake output attribute `packages`.";
      };

      checks = mkOption {
        type = lazyAttrsOf package;
        description = mdDoc "The flake output attribute `checks`.";
      };

      legacyPackages = mkOption {
        type = lazyAttrsOf unspecified;
        description = mdDoc "The flake output attribute `legacyPackages`.";
      };

      devShells = mkOption {
        type = lazyAttrsOf package;
        description = mdDoc "The flake output attribute `devShells`.";
      };

      apps = mkOption {
        type = lazyAttrsOf unspecified;
        description = mdDoc "The flake output attribute `apps`.";
      };

    };

  };

  config = {

    output = {
      final = mkDefault (recursiveUpdate transformed amended);
    };

    outputs = {

      packages = {
        min = main.${config.main}.min;
        default = main.${config.main};
      } // main // extraChecks;

      checks = main // extraChecks;

      legacyPackages = {
        inherit project config;
        inherit (project) pkgs ghc;
        ghcid = config.ghcid;
        shell = config.ghcid.shell;
        show-config = show-config.shell;
        ghcid-run = ghcid-test.shell;
      };

      devShells = mapAttrs (_: s: s.derivation) config.shells // { default = config.ghcid.shell; };

      apps = let
        app = program: { type = "app"; program = "${program}"; };
        ghcid = ghcid-test.app;
      in config.ghcid.apps // config.hackage.output.apps // config.hpack.apps main // {
        inherit ghcid;
        hls = app "${config.shell.hls.app}";
        gen-cabal = app "${config.hpack.script}";
        gen-cabal-quiet = app "${config.hpack.scriptQuiet}";
        hpack = app "${config.hpack.script}";
        hpack-quiet = app "${config.hpack.scriptQuiet}";
        tags = app "${tags.app}";
        show-config = show-config.app;
        cli = app "${config.internal.hixCli.package}/bin/hix";
        c = mapAttrs (_: c: app "${c.path}") config.commands;
        ghcid-new = app config.ghcid.new;
      };

    };

  };
}
