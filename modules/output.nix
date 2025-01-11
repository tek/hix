{ lib, config, util, outputs, ... }:
let
  inherit (lib) optionalAttrs mkOption types;
  inherit (util) app;

  tags = import ../lib/tags.nix { inherit config util; };

  genOverrides = import ../lib/gen-overrides.nix { inherit config lib util; };

  showOverrides = import ../lib/show-overrides.nix { inherit config lib util; };

  depVersions = env: import ../lib/dep-versions.nix { inherit config lib util env; };

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

      expose = {

        appimage = lib.mkOption {
          description = "Include AppImage derivations for all executables in the outputs.";
          type = types.bool;
          default = true;
        };

        internals = lib.mkOption {
          description = "Include the config set, GHC packages and other misc data in the outputs.";
          type = types.bool;
          default = true;
        };

        managed = lib.mkOption {
          description = ''
          Include apps for managed dependencies in the outputs, even as stubs if the feature is disabled.
          '';
          type = types.bool;
          default = true;
        };

        cross = lib.mkOption {
          description = "Include full cross-compilation system sets in the outputs (like
          `hix.cross.{mingw32,aarch64-android,...}`).";
          type = types.bool;
          default = true;
        };

      };

      final = mkOption {
        description = ''
        The final flake outputs computed by Hix, defaulting to the set in [outputs](#opt-general-outputs.packages) and
        its siblings.
        May be overriden for unusual customizations.
        '';
        type = types.raw;
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

      basicApps = util.mapValues app {
        gen-cabal = config.hpack.script;
        gen-cabal-quiet = config.hpack.scriptQuiet;
        tags = tags.app;
        show-config = outputs.internals.show-config.app;
        cli = "${config.internal.hixCli.package}/bin/hix";
        gen-overrides = genOverrides.script;
        gen = genAll false;
        gen-quiet = genAll true;
        show-overrides = showOverrides;
        dep-versions = depVersions "dev";
      };

      lowPrio = let

        toApp = name: conf: let
          prog = conf.program or conf;
        in util.zscriptErrBin name ''
        exec ${prog} $*
        '';

        recurseApps = conf:
        lib.mapAttrs toApps (util.removeKeys ["program" "type" "meta"] conf);

        toApps = name: conf:
        recurseApps conf //
        lib.optionalAttrs (conf ? program) (toApp name conf);

        legacyBasic = [
          outputs.internals.legacyPackages
          outputs.envs.legacyPackages
          outputs.packages.legacyPackages
        ];

        legacyApps = [
          outputs.hpack.legacyApps
          config.hackage.output.apps
          outputs.managed.apps
          util.managed.output.apps
          outputs.commands.appsFull
          util.managed.output.gen
          outputs.envs.apps
          outputs.commands.legacyApps
        ];

      in {

        legacyPackages = util.mergeAll (legacyBasic ++ map recurseApps legacyApps);

        apps = util.mergeAllAttrs [
          basicApps
        ];

      };

      highPrio = {
        packages = outputs.packages.packages;

        checks = outputs.packages.checks;

        legacyPackages = {
          overrides = config.exportedOverrides;
          ${prefix}.env = util.managed.output.envGhcs;
        };

        devShells = outputs.envs.shells // { default = outputs.envs.shells.dev; };

        apps =
          util.mergeAll [
            outputs.commands.apps
            (util.removeApp outputs.packages.apps.dev or {})
          ];
      };

      withPrio = name: (lowPrio.${name} or {}) // highPrio.${name};

      withPrefix = name: let
        low = util.mergeAuto lowPrio.${name} { ${prefix} = removeAttrs lowPrio.${name} [prefix]; };
      in optionalAttrs (lib.hasAttr name lowPrio) low // highPrio.${name};

      asDefault = name: util.mapValues lib.mkDefault (withPrefix name);

      withInternal = outputs:
      util.mapValues lib.mkDefault outputs
      //
      { ${util.internalScope} = removeAttrs outputs [prefix]; };

    in {
      packages = asDefault "packages";
      checks = asDefault "checks";
      legacyPackages = withInternal (withPrefix "legacyPackages");
      devShells = asDefault "devShells";
      apps = withInternal (withPrio "apps");
    };

  };
}
