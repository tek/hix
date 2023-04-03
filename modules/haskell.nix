{ config, lib, util, ... }:
with lib;
with types;
let

  global = config;

  ghcModule = import ./ghc.nix { inherit global util; };
  packageModule = import ./package.nix { inherit global util; };
  cabalOptionsModule = import ./cabal-options.nix { inherit global util; };

  compatProjects = {
    "943" = {};
    "925" = {};
    "902" = {};
    "8107" = { enable = false; };
    "884" = { enable = false; };
  };

  baseFromPackages = let
    pkg = head (attrValues config.internal.packagePaths);
    next = p:
      if p == "/"
      then throw "Could not determine base dir: Invalid package path ${pkg}"
      else if isStorePath p
      then p
      else next (dirOf p);
  in
   if length (attrNames config.internal.packagePaths) == 0
   then throw "You have to specify either the 'base' option, pointing to the project root, or an entry in 'packages'."
   else next pkg;

in {
  options = {

    base = mkOption {
      type = path;
      description = mdDoc "The project's base directory.";
      example = literalExpression "./.";
    };

    packages = mkOption {
      type = attrsOf (submodule packageModule);
      description = mdDoc ''
      The project's Cabal packages, with Cabal package names as keys and package config as values.
      The config is processed with [HPack](https://github.com/sol/hpack).
      Consult the docs for the package options to learn how this is translated.
      '';
      example = literalExpression ''
      {
        spaceship.src = ./.;
        spaceship-api = { src = ./api; dependencies = ["aeson"]; library.enable = true; };
      }
      '';
      default = {};
    };

    main = mkOption {
      type = str;
      description = mdDoc ''
        The name of a key in `packages` that is considered to be the main package.
        This package will be assigned to the `defaultPackage` flake output that is built by a plain
        `nix build`.
      '';
    };

    cabal = mkOption {
      type = unspecified;
      description = mdDoc ''
      Cabal options that are applied to all packages and components.

      If you define any options here, they will be merged with definitions that are set in packages or components.
      This means that the default priority handling applies – definitions in components don't automatically override
      those in packages or the global config.
      You will need to use `mkDefault` or `mkForce`, or even
      `mkOverride` if you define an option at all three levels.

      **Note**: In order to enable cascading of these options, the definitions are not evaluated in-place, but when
      evaluating packages and components. Therefore, referring to these values with e.g.
      `config.cabal.version` does not work as expected if the value uses an option property like
      `mkIf` or `mkOverride`.
      You can use {option}`cabal-config` for this purpose, though.
      '';
      default = {};
    };

    # TODO use readOnly for other instances of this
    cabal-config = mkOption {
      type = submoduleWith { modules = [cabalOptionsModule config.cabal config.internal.cabal-extra]; };
      readOnly = true;
      description = mdDoc ''
      Evaluated version of {option}`cabal`, for referencing in other config values.
      May not be set by the user.
      '';
      default = {};
    };

    auto = mkOption {
      type = bool;
      default = false;
      description = mdDoc ''
        Generate the Cabal file on the fly if none is present in the source directory (or a
        `package.yaml`).
      '';
    };

    forceCabal2nix = mkOption {
      type = bool;
      default = false;
      description = mdDoc "Whether to use cabal2nix even if there is no Cabal file.";
    };

    forceCabalGen = mkOption {
      type = bool;
      default = false;
      description = mdDoc "Whether to generate a Cabal file from Nix config even if there is one in the source directory.";
    };

    ifd = mkOption {
      type = bool;
      default = true;
      description = mdDoc "Whether to use cabal2nix, which uses Import From Derivation, or to generate simple derivations.";
    };

    compiler = mkOption {
      type = str;
      default = "ghc925";
      description = mdDoc ''
        The GHC version used for internal tasks and as default for the dev package set.
      '';
    };

    # TODO migrate
    extraOverrides = mkOption {
      type = lazyAttrsOf (listOf unspecified);
      default = {};
      description = mdDoc ''
        Like `overrides`, but expected to be in normalized form. This allows for extensions of Hix to
        add overrides from multiple locations, since the `listOf` aggregates all definitions.
      '';
    };

    localPackage = mkOption {
      type = unspecified;
      default = _: id;
      description = mdDoc ''
        A function that takes dep combinators and a derivation and returns a modified version of that derivation.
        Called for each cabal2nix derivation of the local packages before inserting it into the overrides.
      '';
      example = literalExpression ''
        { fast, ... }: pkg: fast pkg;
      '';
    };

    profiling = mkOption {
      type = bool;
      default = true;
      description = mdDoc ''
        Global default for whether to build local packages and dependency overrides with profiling enabled.
      '';
    };

    compat = {

      enable = mkOption {
        type = bool;
        default = true;
        description = mdDoc ''
          Create derivations in `outputs.checks` that build the packages with different GHC versions.
          The set of versions is configured by [](#opt-compat-versions).
        '';
      };

      versions = mkOption {
        description = ''
        The GHC versions for which to create compat checks. Defaults to [](#opt-ghcVersion).
        There has to be an env in [](#opt-envs) with the version as its name for each of these.
        '';
        type = listOf str;
        default = config.ghcVersions;
      };

    };

    deps = mkOption {
      type = listOf path;
      default = [];
      description = mdDoc ''
        Flake inputs containing hix projects whose overrides are merged into this project's.
        The `local` overrides are ignored to prevent the dependencies' project packages from being
        injected into the compat checks.
      '';
    };

    depsFull = mkOption {
      type = listOf path;
      default = [];
      description = mdDoc ''
        Flake inputs containing hix projects whose overrides are merged into this project's.
        Unlike `deps`, this includes the `local` overrides.
      '';
    };

    depsProf = mkOption {
      type = listOf path;
      default = [];
      description = mdDoc ''
        Flake inputs containing hix projects whose overrides are merged into this project's.
        Unlike `deps`, this includes the `local` overrides.
        Unlike `depsFull`, the local packages are forced to be built with profiling enabled.
      '';
    };

    pkgs = mkOption {
      type = util.types.pkgs;
      description = mdDoc ''
        The nixpkgs attrset used by the default GHC.
      '';
    };

    internal = {

      basicPkgs = mkOption {
        type = unspecified;
        readOnly = true;
      };

      basicGhc = mkOption {
        type = util.types.ghc;
      };

      packageNames = mkOption {
        type = listOf str;
      };

      packagePaths = mkOption {
        type = attrsOf path;
      };

      relativePackages = mkOption {
        type = attrsOf str;
      };

      cabal-extra = mkOption {
        type = attrsOf unspecified;
        default = {};
      };

      hixCli = {

        ghc = mkOption {
          description = mdDoc "The GHC config used for the Hix CLI, defaulting to the dev GHC without overrides.";
          type = submodule ghcModule;
        };

        overrides = mkOption {
          type = util.types.cabalOverrides;
          description = mdDoc "The overrides used for the CLI client.";
        };

        package = mkOption {
          description =
            "The package for the Hix CLI, defaulting to the local package in the input repository using the dev GHC.";
          type = package;
        };

        exe = mkOption {
          description =
            "The executable in the `bin/` directory of [](#opt-hixCli-package).";
          type = path;
          default = "${config.internal.hixCli.package}/bin/hix";
        };

      };

    };
  };

  config = {
    base = mkDefault baseFromPackages;

    # TODO correct link
    main = mkDefault (
      if (length config.internal.packageNames == 1)
      then head config.internal.packageNames
      else throw ''
      The config option 'main' must name one of the 'packages' if more than one is defined.
      See https://tryp.io/hix/manual/index.html#opt-main
      ''
    );

    pkgs = mkDefault config.envs.dev.ghc.pkgs;

    internal = {

      basicPkgs = import config.inputs."nixpkgs_${config.compiler}" { inherit (config) system; };

      basicGhc = config.internal.basicPkgs.haskell.packages.${config.compiler};

      # TODO extraOverrides is still needed for ribosome etc.
      # overrides = util.mergeOverrides [config.extraOverrides overrides];

      packageNames = attrNames config.packages;

      packagePaths = mapAttrs (_: p: p.src) config.packages;

      relativePackages = util.relativePackages config.base config.internal.packagePaths;

      hixCli = let
        cfg = config.internal.hixCli;
      in {

        overrides = mkDefault (
          {hackage, source, fast, notest, ...}: {
            exon = hackage "1.4.0.0" "1m4i3a14wip985ncblfy2ikcy7gw5rryj9z497ah218d1nmwj7rl";
            flatparse = hackage "0.4.0.2" "0saxwgwbzijgm9v5w9nx3npl28szpkyz97m4shn8yanxq7gsjnvg";
            incipit-base = hackage "0.5.0.0" "02fdppamn00m94xqi4zhm6sl1ndg6lhn24m74w24pq84h44mynl6";
            hix = notest (fast (source.root ../packages/hix));
          }
        );

        # TODO replace this with `inputs.hix` to avoid incompatibility
        ghc = {
          name = "hix";
          compiler = "ghc925";
          nixpkgs = config.envs.dev.ghc.nixpkgs;
          nixpkgsOptions = config.envs.dev.ghc.nixpkgsOptions;
          overrides = mkForce cfg.overrides;
          overlays = config.envs.dev.ghc.overlays;
        };

        package = cfg.ghc.ghc.hix;

      };

    };
  };
}
