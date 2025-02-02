{ lib, config, util, outputs, ... }:
let
  inherit (lib) optionalAttrs mkOption types;
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
        show-config = show-config.appScript;
        cli = "${config.internal.hixCli.package}/bin/hix";
        gen-overrides = genOverrides.script;
        gen = genAll false;
        gen-quiet = genAll true;
        show-overrides = showOverrides;
        dep-versions = depVersions "dev";
      };

      basicEnvApps = {
        env = util.mergeAll [
          outputs.commands.appsFull
          outputs.packages.apps
        ];
      };

      lowPrio = {

        legacyPackages =
          util.mergeAll [
            {
              inherit config;
              inherit (config.envs.dev.ghc) pkgs ghc;
              ghc0 = config.envs.dev.ghc.vanillaGhc;
              show-config = show-config.shell;
            }
            outputs.envs.legacyPackages
            outputs.packages.legacyPackages
          ];

        apps = util.mergeAllAttrs [
          outputs.hpack.legacyApps
          config.hackage.output.apps
          basicApps
          outputs.managed.apps
          util.managed.output.apps
          basicEnvApps
          util.managed.output.gen
          outputs.packages.apps
          outputs.envs.apps
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
            (util.removeApp outputs.packages.apps.dev)
          ];
      };

      withPrio = name: let
        low = util.mergeAuto lowPrio.${name} { ${prefix} = removeAttrs lowPrio.${name} [prefix]; };
        full = optionalAttrs (lib.hasAttr name lowPrio) low // highPrio.${name};
      in full // { ${util.internalScope} = removeAttrs full [prefix]; };

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
