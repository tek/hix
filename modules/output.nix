{ lib, config, util, outputs, ... }:
let
  inherit (lib) optionalAttrs mkOption types;

  tags = import ../lib/tags.nix { inherit config util; };

  genOverrides = import ../lib/gen-overrides.nix { inherit config lib util; };

  showOverrides = import ../lib/show-overrides.nix { inherit util; };

  depVersions = env: import ../lib/dep-versions.nix { inherit config lib util env; };

  genAll = quiet: util.script "hix-gen-all" ''
  ${if quiet then config.hpack.scriptQuiet else config.hpack.script}
  ${if config.gen-overrides.enable then genOverrides.script else ""}
  '';

in {
  options = {

    output = {

      extraPackages = mkOption {
        description = ''
        Names of packages that will be added to the flake outputs, despite not being declared in
        [](#opt-general-packages).

        This may be a simple Hackage package like `aeson` or a local package that is added in
        [](#opt-general-overrides) due to the way its source is obtained.
        '';
        type = types.listOf types.str;
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
          description = ''
          Include full cross-compilation system sets in the outputs (like `hix.cross.{mingw32,aarch64-android,...}`).
          '';
          type = types.bool;
          default = true;
        };

        static = lib.mkOption {
          description = ''
          Include static executables in the outputs, built with `musl`.
          '';
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
        type = types.lazyAttrsOf types.package;
      };

      checks = mkOption {
        description = "The flake output attribute `checks`.";
        type = types.lazyAttrsOf types.package;
      };

      legacyPackages = mkOption {
        description = "The flake output attribute `legacyPackages`.";
        type = types.lazyAttrsOf types.raw;
      };

      devShells = mkOption {
        description = "The flake output attribute `devShells`.";
        type = types.lazyAttrsOf types.package;
      };

      apps = mkOption {
        description = "The flake output attribute `apps`.";
        type = types.lazyAttrsOf util.types.flakeApp;
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

      lowPrio = {

        legacyPackages = util.mergeAll [
          outputs.internals.legacyPackages
          outputs.envs.legacyPackages
          outputs.packages.legacyPackages
          outputs.hackage.legacyPackages
          outputs.commands.legacyPackages
          outputs.managed.legacyPackages
          outputs.hpack.deprecatedApps
        ];

        apps = util.mapValues util.app basicApps;

      };

      highPrio = {
        packages = outputs.packages.packages;

        legacyPackages = {
          overrides = config.exportedOverrides;
          ${prefix}.env = util.managed.output.envGhcs;
          ${util.internalScope} =
            lib.mapAttrs util.ensureLegacyApp basicApps
            //
            { cli-context = outputs.cli-context; }
            //
            outputs.envs.internal
            ;
        };

        devShells = outputs.envs.shells // { default = outputs.envs.shells.dev; };

        apps =
          util.mergeAll [
            outputs.commands.apps
            (util.removeApp outputs.packages.apps.dev or {})
            outputs.hackage.apps
          ];
      };

      withPrio = name: (lowPrio.${name} or {}) // highPrio.${name};

      withPrefix = name: let
        low = util.mergeAuto lowPrio.${name} { ${prefix} = removeAttrs lowPrio.${name} [prefix]; };
      in optionalAttrs (lib.hasAttr name lowPrio) low // highPrio.${name};

      asDefault = lib.mapAttrs (name: if name == util.internalScope then lib.id else lib.mkDefault);

      withInternal = outputs:
      util.mergeAll [
        outputs
        { ${util.internalScope} = removeAttrs outputs [util.internalScope]; }
      ];

    in util.mapValues asDefault {
      packages = withPrefix "packages";
      checks = outputs.packages.checks;
      legacyPackages = withInternal (withPrefix "legacyPackages");
      devShells = withPrefix "devShells";
      apps = withPrio "apps";
    };

  };
}
