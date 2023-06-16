{ config, lib, util, ... }:
with lib;
with types;
let

  global = config;

  ghcModule = import ./ghc.nix { inherit global util; };
  packageModule = import ./package.nix { inherit global util; };
  cabalOptionsModule = import ./cabal-options.nix { inherit global util; };

  baseFromPackages = let
    pkg = head (attrValues config.internal.packagePaths);
    next = p:
      if p == "/"
      then throw "Could not determine base dir: Invalid package path ${pkg}"
      else if isStorePath p || pathExists "${p}/flake.nix"
      then p
      else next (dirOf p);
  in
   if length (attrNames config.internal.packagePaths) == 0
   then throw "You have to specify either the 'base' option, pointing to the project root, or an entry in [](#opt-general-packages)."
   else next pkg;

in {
  options = {

    base = mkOption {
      description = mdDoc ''
      The project's base directory.

      Will be inferred from package source directories if unset.
      If the Hix project is a subdirectory of a git repository and `flake.nix` isn't tracked by git, this option has to
      be set manually to `./.`.
      '';
      example = literalExpression "./.";
      type = path;
    };

    packages = mkOption {
      description = mdDoc ''
      The project's Cabal packages, with Cabal package names as keys and package config as values.
      The config is processed with [HPack](https://github.com/sol/hpack).
      Consult the docs for the package options to learn how this is translated.
      '';
      example = literalExpression ''
      {
        core.src = ./.;
        api = { src = ./api; cabal.dependencies = ["aeson"]; library.enable = true; };
      }
      '';
      type = attrsOf (submodule packageModule);
      default = {};
    };

    main = mkOption {
      description = mdDoc ''
        The name of a key in `packages` that is considered to be the main package.
        This package will be assigned to the `defaultPackage` flake output that is built by a plain
        `nix build`.
      '';
      type = str;
    };

    cabal = mkOption {
      description = mdDoc ''
      Cabal options that are applied to all packages and components.

      If you define any options here, they will be merged with definitions that are set in packages or components.
      This means that the default priority handling applies – definitions in components don't automatically override
      those in packages or the global config.
      You will need to use `mkDefault` or `mkForce`, or even
      `mkOverride` if you define an option at all three levels.

      ::: {.note}
      In order to enable cascading for these options, the definitions are not evaluated in-place, but when evaluating
      packages and components. Therefore, referring to these values with e.g. `config.cabal.version` does not work as
      expected if the value uses an option property like `mkIf` or `mkOverride`.
      You can use {option}`cabal-config` for this purpose, though.
      :::
      '';
      type = deferredModule;
      default = {};
    };

    cabal-config = mkOption {
      description = mdDoc ''
      Evaluated version of {option}`cabal`, for referencing in other config values.
      May not be set by the user.
      '';
      type = submoduleWith { modules = [cabalOptionsModule config.cabal config.internal.cabal-extra]; };
      readOnly = true;
      default = {};
    };

    auto = mkOption {
      description = mdDoc ''
      Generate Cabal files on the fly if none are present in source directories (or a `package.yaml`).
      '';
      type = bool;
      default = false;
    };

    # TODO cli must be able to resolve components from metadata by reading cabal.project and *.cabal
    manualCabal = mkOption {
      description = mdDoc ''
      Don't use the options in [](#opt-general-packages) as Cabal configuration for the ghci preprocessor.
      '';
      type = bool;
      default = false;
    };

    forceCabal2nix = mkOption {
      description = mdDoc "Whether to use cabal2nix even if there is no Cabal file.";
      type = bool;
      default = false;
    };

    forceCabalGen = mkOption {
      description = mdDoc "Whether to generate a Cabal file from Nix config even if there is one in the source directory.";
      type = bool;
      default = false;
    };

    ifd = mkOption {
      description = mdDoc ''
      Whether to use cabal2nix, which uses Import From Derivation, or to generate simple derivations, for local
      packages.
      '';
      type = bool;
      default = false;
    };

    compiler = mkOption {
      description = mdDoc ''
        The GHC version used for internal tasks and as default for the default environment.
      '';
      type = str;
      default = "ghc92";
    };

    compat = {

      enable = mkOption {
        description = mdDoc ''
          Create derivations in [](#opt-general-outputs.checks) that build the packages with different GHC versions.
          The set of versions is configured by [](#opt-general-compat.versions).
        '';
        type = bool;
        default = true;
      };

      versions = mkOption {
        description = mdDoc ''
        The GHC versions for which to create compat checks. Defaults to [](#opt-general-ghcVersions).
        There has to be an env in [](#opt-general-envs) with the version as its name for each of these.
        '';
        type = listOf str;
        default = config.ghcVersions;
      };

      ifd = mkOption {
        description = mdDoc ''
        Whether to allow IFD for compat checks.
        '';
        type = bool;
        default = config.ifd;
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
      readOnly = true;
    };

    internal = {

      pkgs = mkOption {
        type = unspecified;
        readOnly = true;
      };

      basicGhc = mkOption {
        type = util.types.ghc;
        readOnly = true;
      };

      packageNames = mkOption {
        type = listOf str;
        readOnly = true;
      };

      packagePaths = mkOption {
        type = attrsOf path;
        readOnly = true;
      };

      relativePackages = mkOption {
        type = attrsOf str;
        readOnly = true;
      };

      cabal-extra = mkOption {
        type = attrsOf unspecified;
        default = {};
      };

      hixVersion = mkOption {
        description = mdDoc "The Hix version used by this project.";
        type = str;
        readOnly = true;
      };

      hixUrl = mkOption {
        description = mdDoc "The URL to the Hix release tag used by this project.";
        type = str;
        default = "github:tek/hix?ref=${config.internal.hixVersion}";
      };

    };
  };

  config = {
    base = mkDefault baseFromPackages;

    main = mkDefault (
      if (length config.internal.packageNames == 1)
      then head config.internal.packageNames
      else throw ''
      The config option 'main' must name one of the 'packages' if more than one is defined.
      See https://tryp.io/hix/manual/index.html#opt-general-main
      ''
    );

    pkgs = mkDefault config.envs.dev.ghc.pkgs;

    internal = {

      pkgs = import config.inputs.nixpkgs { inherit (config) system; };

      basicGhc = config.internal.pkgs.haskell.packages.${config.compiler};

      packageNames = attrNames config.packages;

      packagePaths = mapAttrs (_: p: p.src) config.packages;

      relativePackages = util.relativePackages config.base config.internal.packagePaths;

      hixVersion = "0.5.4";

    };
  };
}
