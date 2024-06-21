{ lib, config, util, ... }:
let
  inherit (lib) optionalAttrs mkOption;
  inherit (util) app;

  tags = import ../lib/tags.nix { inherit config util; };

  showConfig = import ../lib/show-config.nix { inherit config lib util; };

  genOverrides = import ../lib/gen-overrides.nix { inherit config lib util; };

  showOverrides = import ../lib/show-overrides.nix { inherit config lib util; };

  depVersions = env: import ../lib/dep-versions.nix { inherit config lib util env; };

  # TODO use the json method and print in cli
  show-config = util.paramApp {
    name = "show-config";
    func = showConfig;
    params = ["path"];
  };

  genAll = quiet: util.script "hix-gen-all" ''
  ${if quiet then config.hpack.scriptQuiet else config.hpack.script}
  ${if config.gen-overrides.enable then genOverrides.script else ""}
  '';

in {
  options = with lib.types; {

    output = {

      extraPackages = mkOption {
        description = ''
        Names of packages that will be added to the flake outputs, despite not being declared in
        [](#opt-general-packages).

        This may be a simple Hackage package like `aeson` or a local package that is added in
        [](#opt-general-overrides) due to the way its source is obtained.
        '';
        type = listOf str;
        default = [];
      };

      final = mkOption {
        description = ''
        The final flake outputs computed by Hix, defaulting to the set in `outputs`.
        May be overriden for unusual customizations.
        '';
        type = unspecified;
      };

      commandApps = mkOption {
        description = ''
        Whether to expose all commands in the attrset `apps.cmd.*`.

        This means that you can run `nix run .#cmd.do-stuff` to access the command defined as `commands.do-stuff`, but
        it does not strictly conform to the convention for `apps`, potentially breaking other tooling.
        '';
        type = bool;
        default = true;
      };

      envApps = mkOption {
        description = ''
        Whether to expose all commands in the attrset `apps.env.<env>.*`.

        Like [](#opt-general-output.commandApps), but commands are executed in the selected environment.
        '';
        type = bool;
        default = true;
      };

    };

    outputs = {

      packages = mkOption {
        description = "The flake output attribute `packages`.";
        type = lazyAttrsOf raw;
      };

      checks = mkOption {
        description = "The flake output attribute `checks`.";
        type = lazyAttrsOf raw;
      };

      legacyPackages = mkOption {
        description = "The flake output attribute `legacyPackages`.";
        type = lazyAttrsOf raw;
      };

      devShells = mkOption {
        description = "The flake output attribute `devShells`.";
        type = lazyAttrsOf raw;
      };

      apps = mkOption {
        description = "The flake output attribute `apps`.";
        type = util.types.flakeAppRec;
      };

    };

  };

  config = {

    output = {
      final = lib.mkDefault config.outputs;
    };

    outputs = let

      prefix = config.buildOutputsPrefix;

      basicApps = {
        hpack = app "${config.hpack.script}";
        hpack-quiet = app "${config.hpack.scriptQuiet}";
        tags = app tags.app;
        show-config = show-config.app;
        cli = app "${config.internal.hixCli.package}/bin/hix";
        gen-overrides = app "${genOverrides.script}";
        gen = app "${genAll false}";
        gen-quiet = app "${genAll true}";
        show-overrides = app "${showOverrides}";
        dep-versions = app "${depVersions "dev"}";
      };

      basicEnvApps = optionalAttrs config.output.envApps {
        env = util.mapListCatAttrs util.output.envApps (lib.attrValues util.visibleAppEnvs);
      };

      lowPrio = {

        legacyPackages =
          util.output.scopedEnvOutputs (["dev"] ++ config.ghcVersions) //
          util.output.envsApi config.envs //
          {
            inherit config;
            inherit (config.envs.dev.ghc) pkgs ghc;
            ghc0 = config.envs.dev.ghc.vanillaGhc;
            show-config = show-config.shell;
          };

        apps = config.hackage.output.apps //
        basicApps //
        util.managed.output.apps //
        util.output.mainAppimageApp //
        optionalAttrs config.output.commandApps { cmd = util.output.commandApps config.commands; } //
        basicEnvApps //
        util.managed.output.gen
        ;

      };

      highPrio = {
        packages = util.output.devPackages // util.output.prefixedInEnvs "packages" config.ghcVersions;

        checks =
          util.output.envOutputs "checks" "dev" //
          optionalAttrs config.compat.enable (util.output.prefixedInEnvs "compat" config.compat.versions) //
          util.managed.output.checks
          ;

        legacyPackages = {
          overrides = config.exportedOverrides;
          ${prefix}.env = util.managed.output.envGhcs;
        };

        devShells = let
          envShells = lib.mapAttrs (_: e: e.shell) (lib.filterAttrs (_: util.envSystemAllowed) util.visibleEnvs);
        in envShells // { default = envShells.dev; };

        apps = let
          exposedCommands = lib.filterAttrs (_: c: c.expose) config.commands;
        in
          util.output.commandApps exposedCommands //
          {
            gen-cabal = app "${config.hpack.script}";
            gen-cabal-quiet = app "${config.hpack.scriptQuiet}";
          } //
          config.hpack.apps;
      };

      withPrio = name: let
        low = util.mergeAuto lowPrio.${name} { ${prefix} = removeAttrs lowPrio.${name} [prefix]; };
      in optionalAttrs (lib.hasAttr name lowPrio) low // highPrio.${name};

      asDefault = name: util.mapValues lib.mkDefault (withPrio name);

    in {
      packages = asDefault "packages";
      checks = asDefault "checks";
      legacyPackages = asDefault "legacyPackages";
      devShells = asDefault "devShells";
      apps = asDefault "apps";
    };

  };
}
