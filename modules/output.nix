{ lib, config, util, ... }:
with lib;
with types;
let

  envCommand = import ../lib/command.nix { inherit config util; };

  project = config.envs.dev.ghc;

  outPackagesFor = packages: ghc:
  genAttrs packages (n: ghc.${n} // { inherit ghc; });

  compatChecks = let
    prefixed = conf: mapAttrs' (n: v: { name = "${conf.ghc.compiler}-${n}"; value = v; });
    compatCheck = ver: let
      conf = config.envs.${ver};
    in
    optionalAttrs conf.enable
    (prefixed conf (outPackagesFor config.internal.packageNames conf.ghc.ghc));
  in
    util.foldMapAttrs compatCheck config.ghcVersions;

  releaseDrv =
    import ../lib/release-derivation.nix {
      inherit lib;
      hsLib = config.pkgs.haskell.lib;
    };

  base = outPackagesFor (config.internal.packageNames ++ config.output.extraPackages) project.ghc;

  main = let
    extra = n: base.${n} // { min = config.envs.min.ghc.ghc.${n}; release = releaseDrv base.${n}; };
  in
    base // genAttrs config.internal.packageNames extra;

  extraChecks = if config.compat.enable then compatChecks else {};

  transformed = config.output.transform main config.outputs;

  amended = config.output.amend main transformed;

  tags = import ../lib/tags.nix { inherit config; };

  showConfig = import ../lib/show-config.nix { inherit config lib util; };

  # TODO use the json method and print in cli
  show-config = util.paramApp {
    name = "show-config";
    func = showConfig;
    params = ["path"];
  };

  app = program: { type = "app"; program = "${program}"; };

  # TODO add hls (needs ghc in env)
  envApps = env: {
    ${env.name} = mapAttrs (_: command: app "${(envCommand { inherit env command; }).path}") config.commands;
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
        inherit config;
        inherit (config.envs.dev.ghc) pkgs ghc;
        show-config = show-config.shell;
      };

      devShells = mapAttrs (_: s: s.derivation) config.shells // { default = config.ghcid.shell; };

      apps = config.ghcid.apps // config.hackage.output.apps // config.hpack.apps main // {
        hls = app "${config.shell.hls.app}";
        gen-cabal = app "${config.hpack.script}";
        gen-cabal-quiet = app "${config.hpack.scriptQuiet}";
        hpack = app "${config.hpack.script}";
        hpack-quiet = app "${config.hpack.scriptQuiet}";
        tags = app tags.app;
        show-config = show-config.app;
        cli = app "${config.internal.hixCli.package}/bin/hix";
        c = mapAttrs (_: c: app "${c.path}") config.commands;
        env = util.foldMapAttrs envApps (attrValues config.envs);
        ghcid = app config.ghcid.flakeApp;
        ghci = app config.ghci.flakeApp;
      };

    };

  };
}
