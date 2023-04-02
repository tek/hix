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

  extraChecks = if config.compat.enable then compatChecks else {};

  tags = import ../lib/tags.nix { inherit config; };

  showConfig = import ../lib/show-config.nix { inherit config lib util; };

  # TODO use the json method and print in cli
  show-config = util.paramApp {
    name = "show-config";
    func = showConfig;
    params = ["path"];
  };

  app = program: { type = "app"; program = "${program}"; };

  # TODO add hls
  envApps = env: {
    ${env.name} = mapAttrs (_: command: app "${(envCommand { inherit env command; }).path}") config.commands;
  };

in {
  options = {

    # TODO copy this to env and reference dev here
    # XXX this is most significant for abstracting away from a static, privileged dev env
    derivations = mkOption {
      description = mdDoc "The derivations for the local Cabal packages.";
      type = lazyAttrsOf package;
      default = let
        extra = n: base.${n} // { min = config.envs.min.ghc.ghc.${n}; release = releaseDrv base.${n}; };
      in base // genAttrs config.internal.packageNames extra;
    };

    output = {

      systems = mkOption {
        type = listOf str;
        description = mdDoc "The systems for which to create outputs.";
        default = ["x86_64-linux"];
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
        description = mdDoc ''
        The final flake outputs computed by Hix, defaulting to {#opt-outputs}.
        May be overriden for unusual customizations.
        '';
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
      final = mkDefault config.outputs;
    };

    outputs = {

      packages = {
        min = config.derivations.${config.main}.min;
        default = config.derivations.${config.main};
      } // config.derivations // extraChecks;

      checks = config.derivations // extraChecks;

      legacyPackages = {
        inherit config;
        inherit (config.envs.dev.ghc) pkgs ghc;
        show-config = show-config.shell;
      };

      devShells = mapAttrs (_: s: s.derivation) config.shells // { default = config.ghcid.shell; };

      apps = let
        commands = mapAttrs (_: c: app "${c.path}") config.commands;
      in config.hackage.output.apps // config.hpack.apps // {
        hls = app "${config.shell.hls.app}";
        gen-cabal = app "${config.hpack.script}";
        gen-cabal-quiet = app "${config.hpack.scriptQuiet}";
        hpack = app "${config.hpack.script}";
        hpack-quiet = app "${config.hpack.scriptQuiet}";
        tags = app tags.app;
        show-config = show-config.app;
        cli = app "${config.internal.hixCli.package}/bin/hix";
        c = commands;
        env = util.foldMapAttrs envApps (attrValues config.envs);
        ghcid = commands.ghcid;
        ghci = commands.ghci;
      };

    };

  };
}
