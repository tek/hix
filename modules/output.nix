{ lib, config, util, ... }:
with lib;
with types;
let

  envCommand = import ../lib/command.nix { inherit config util; };

  tags = import ../lib/tags.nix { inherit config; };

  showConfig = import ../lib/show-config.nix { inherit config lib util; };

  libOutput = import ../lib/output.nix { inherit config lib util; };

  # TODO use the json method and print in cli
  show-config = util.paramApp {
    name = "show-config";
    func = showConfig;
    params = ["path"];
  };

  app = program: { type = "app"; program = "${program}"; };

  envApps = env: {
    ${env.name} = mapAttrs (_: command: app "${(envCommand { inherit env command; }).path}") config.commands;
  };

  commandApps = mapAttrs (_: c: app "${c.path}");

in {
  options = {

    output = {

      extraPackages = mkOption {
        description = mdDoc ''
        Names of packages that will be added to the flake outputs, despite not being declared in
        [](#opt-general-packages).
        This may be a simple Hackage package like `aeson` or a local package that is added in
        [](#opt-general-overrides) due to the way its source is obtained.
        '';
        type = listOf str;
        default = [];
      };

      final = mkOption {
        description = mdDoc ''
        The final flake outputs computed by Hix, defaulting to the set in `outputs`.
        May be overriden for unusual customizations.
        '';
        type = unspecified;
      };

    };

    outputs = {

      packages = mkOption {
        description = mdDoc "The flake output attribute `packages`.";
        type = lazyAttrsOf unspecified;
      };

      checks = mkOption {
        description = mdDoc "The flake output attribute `checks`.";
        type = lazyAttrsOf unspecified;
      };

      legacyPackages = mkOption {
        description = mdDoc "The flake output attribute `legacyPackages`.";
        type = lazyAttrsOf unspecified;
      };

      devShells = mkOption {
        description = mdDoc "The flake output attribute `devShells`.";
        type = unspecified;
      };

      apps = mkOption {
        description = mdDoc "The flake output attribute `apps`.";
        type = lazyAttrsOf unspecified;
      };

    };

  };

  config = {

    output = {
      final = mkDefault config.outputs;
    };

    outputs = {

      packages = libOutput.devOutputs // libOutput.envDerivations config.ghcVersions;

      checks =
        config.envs.dev.derivations //
        optionalAttrs config.compat.enable (libOutput.envDerivations config.compat.versions);

      legacyPackages = {
        inherit config;
        inherit (config.envs.dev.ghc) pkgs ghc;
        show-config = show-config.shell;
        overrides = config.exportedOverrides;
      };

      devShells = let

        shells = mapAttrs (_: e: e.shell) util.visibleEnvs;

      in shells // { default = shells.dev; };

      apps = let

        exposed = filterAttrs (_: c: c.expose) config.commands;

      in config.hackage.output.apps // config.hpack.apps // commandApps exposed // {
        gen-cabal = app "${config.hpack.script}";
        gen-cabal-quiet = app "${config.hpack.scriptQuiet}";
        hpack = app "${config.hpack.script}";
        hpack-quiet = app "${config.hpack.scriptQuiet}";
        tags = app tags.app;
        show-config = show-config.app;
        cli = app "${config.internal.hixCli.package}/bin/hix";
        cmd = commandApps config.commands;
        env = util.foldMapAttrs envApps (attrValues util.visibleEnvs);
      };

    };

  };
}
