{ lib, config, util, ... }:
let
  inherit (lib) optionalAttrs mkOption mdDoc;
  inherit (util) app;

  tags = import ../lib/tags.nix { inherit config; };

  showConfig = import ../lib/show-config.nix { inherit config lib util; };

  libOutput = import ../lib/output.nix { inherit config lib util; };

  genOverrides = import ../lib/gen-overrides.nix { inherit config lib util; };

  showOverrides = import ../lib/show-overrides.nix { inherit config lib util; };

  depVersions = env: import ../lib/dep-versions.nix { inherit config lib util env; };

  # TODO use the json method and print in cli
  show-config = util.paramApp {
    name = "show-config";
    func = showConfig;
    params = ["path"];
  };

  genAll = config.pkgs.writeScript "hix-gen-all" ''
  ${config.hpack.script}
  ${if config.gen-overrides.enable then genOverrides else ""}
  '';

in {
  options = with lib.types; {

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

      commandApps = mkOption {
        description = mdDoc ''
        Whether to expose all commands in the attrset `apps.cmd.*`.

        This means that you can run `nix run .#cmd.do-stuff` to access the command defined as `commands.do-stuff`, but
        it does not strictly conform to the convention for `apps`, potentially breaking other tooling.
        '';
        type = bool;
        default = true;
      };

      envApps = mkOption {
        description = mdDoc ''
        Whether to expose all commands in the attrset `apps.env.<env>.*`.

        Like [](#opt-general-output.commandApps), but commands are executed in the selected environment.
        '';
        type = bool;
        default = true;
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
      final = lib.mkDefault config.outputs;
    };

    outputs = {

      packages = libOutput.devOutputs // libOutput.prefixedEnvDerivations config.ghcVersions;

      checks =
        config.envs.dev.derivations //
        optionalAttrs config.compat.enable (libOutput.prefixedEnvDerivations config.compat.versions);

      legacyPackages = libOutput.scopedEnvDerivations config.ghcVersions // libOutput.envsApi config.envs // {
        inherit config;
        inherit (config.envs.dev.ghc) pkgs ghc;
        show-config = show-config.shell;
        overrides = config.exportedOverrides;
      };

      devShells = let

        shells = lib.mapAttrs (_: e: e.shell) (lib.filterAttrs (_: util.envSystemAllowed) util.visibleEnvs);

      in shells // { default = shells.dev; };

      apps = let

        exposed = lib.filterAttrs (_: c: c.expose) config.commands;

      in config.hackage.output.apps // libOutput.commandApps exposed // {
        gen-cabal = app "${config.hpack.script}";
        gen-cabal-quiet = app "${config.hpack.scriptQuiet}";
        hpack = app "${config.hpack.script}";
        hpack-quiet = app "${config.hpack.scriptQuiet}";
        tags = app tags.app;
        show-config = show-config.app;
        cli = app "${config.internal.hixCli.package}/bin/hix";
        gen-overrides = app "${genOverrides}";
        gen = app "${genAll}";
        show-overrides = app "${showOverrides}";
        dep-versions = app "${depVersions "dev"}";
      } // libOutput.mainAppimageApp // optionalAttrs config.output.commandApps {
        cmd = libOutput.commandApps config.commands;
      } // optionalAttrs config.output.envApps {
        env = util.foldMapAttrs libOutput.envApps (lib.attrValues util.visibleEnvs);
      } // config.hpack.apps;

    };

  };
}
