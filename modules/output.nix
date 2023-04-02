{ lib, config, util, ... }:
with lib;
with types;
let

  envCommand = import ../lib/command.nix { inherit config util; };

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

  versionDerivations = genAttrs config.ghcVersions (v: config.envs.${v}.derivations);

  releaseDrv = import ../lib/release-derivation.nix {
    inherit lib;
    hsLib = config.pkgs.haskell.lib;
  };

  devOutputs = let
    ghc = config.envs.dev.ghc.ghc;
    minGhc = config.envs.min.ghc.ghc;
    package = name: ghc.${name} // { release = releaseDrv ghc.${name}; min = minGhc.${name}; };
    local = genAttrs config.internal.packageNames package;
  in local // {
    default = local.${global.main};
    min = local.${global.main}.min;
  };

in {
  options = {

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
        type = util.types.nestedPackages;
        description = mdDoc "The flake output attribute `packages`.";
      };

      checks = mkOption {
        type = util.types.nestedPackages;
        description = mdDoc "The flake output attribute `checks`.";
      };

      legacyPackages = mkOption {
        type = lazyAttrsOf unspecified;
        description = mdDoc "The flake output attribute `legacyPackages`.";
      };

      devShells = mkOption {
        type = util.types.nestedPackages;
        description = mdDoc "The flake output attribute `devShells`.";
      };

      apps = mkOption {
        type = util.types.nestedFlakeApps;
        description = mdDoc "The flake output attribute `apps`.";
      };

    };

  };

  config = {

    output = {
      final = mkDefault config.outputs;
    };

    outputs = {

      packages = devOutputs // versionDerivations;

      checks = config.envs.dev.derivations // optionalAttrs config.compat.enable versionDerivations;

      legacyPackages = {
        inherit config;
        inherit (config.envs.dev.ghc) pkgs ghc;
        show-config = show-config.shell;
      };

      # TODO
      devShells = mapAttrs (_: s: s.derivation) config.shells // { default = throw "not implemented"; };

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
