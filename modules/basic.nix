{ config, lib, util, internal, ... }:
with lib;
with types;
let

  global = config;

  packageModule = import ./package.nix { inherit global util; };
  cabalOptionsModule = import ./cabal-options.nix { inherit global util; };

in {
  options = {

    base = mkOption {
      description = ''
      The project's base directory.

      Will be inferred from package source directories if unset.
      If the Hix project is a subdirectory of a git repository and `flake.nix` isn't tracked by git, this option has to
      be set to `./.` explicitly.
      '';
      example = literalExpression "./.";
      type = types.nullOr path;
      default = null;
    };

    packages = mkOption {
      description = ''
      The project's Cabal packages, with Cabal package names as keys and package config as values.
      The config is processed with [HPack](https://github.com/sol/hpack).
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
      description = ''
        The name of a key in `packages` that is considered to be the main package.
        This package will be assigned to the `defaultPackage` flake output that is built by a plain
        `nix build`.
        If this option is undefined, Hix will choose one of the packages that are not in the dependencies of any other
        package.
      '';
      type = str;
    };

    name = mkOption {
      description = ''
      The name of the project is used for some temporary directories and defaults to [](#opt-general-main).
      If no packages are defined, a dummy is used if possible.
      '';
      type = types.str;
      default = "hix-project";
    };

    cabal = mkOption {
      description = ''
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
      description = ''
      Evaluated version of {option}`cabal`, for referencing in other config values.
      May not be set by the user.
      '';
      type = submoduleWith { modules = [cabalOptionsModule config.cabal config.internal.cabal-extra]; };
      readOnly = true;
      default = {};
    };

    # TODO cli must be able to resolve components from metadata by reading cabal.project and *.cabal
    manualCabal = mkOption {
      description = ''
      Don't use the options in [](#opt-general-packages) as Cabal configuration for the ghci preprocessor.
      '';
      type = bool;
      default = false;
    };

    forceCabal2nix = mkOption {
      description = "Whether to use cabal2nix even if there is no Cabal file.";
      type = bool;
      default = false;
    };

    # TODO is this effective and desired?
    forceCabalGen = mkOption {
      description = ''
      Whether to generate a Cabal file from Nix config even if there is one in the source directory.
      '';
      type = bool;
      default = false;
    };

    ifd = mkOption {
      description = ''
      Whether to use `cabal2nix`, which uses Import From Derivation, or to generate simple derivations, for local
      packages.
      '';
      type = bool;
      default = false;
    };

    compiler = mkOption {
      description = ''
      The GHC version used for internal tasks and for the default environment.
      This is an attribute name in the nixpkgs set `haskell.packages`, which is usually in the format `ghc96`.
      '';
      type = str;
      default = "ghc98";
    };

    compat = {

      enable = mkOption {
        description = ''
          Create derivations in [](#opt-general-outputs.checks) that build the packages with different GHC versions.
          The set of versions is configured by [](#opt-general-compat.versions).
        '';
        type = bool;
        default = true;
      };

      versions = mkOption {
        description = ''
        The GHC versions for which to create compat checks. Defaults to [](#opt-general-ghcVersions).
        There has to be an env in [](#opt-general-envs) with the version as its name for each of these.
        '';
        type = listOf str;
        default = config.ghcVersions;
      };

      ifd = mkOption {
        description = ''
        Whether to allow IFD for compat checks.
        '';
        type = bool;
        default = config.ifd;
      };

    };

    deps = mkOption {
      type = listOf path;
      default = [];
      description = ''
        Flake inputs containing hix projects whose overrides are merged into this project's.
        The `local` overrides are ignored to prevent the dependencies' project packages from being
        injected into the compat checks.
      '';
    };

    depsFull = mkOption {
      type = listOf path;
      default = [];
      description = ''
        Flake inputs containing hix projects whose overrides are merged into this project's.
        Unlike `deps`, this includes the `local` overrides.
      '';
    };

    haskellTools = mkOption {
      description = ''
      Function returning a list of names of Haskell packages that should be included in every environment's `$PATH`.
      This is a convenience variant of [](#opt-env-buildInputs) that provides the environment's GHC package set (without
      overrides) as a function argument.
      This is intended for tooling like `fourmolu`.
      The default consists of `cabal-install`, since that's a crucial tool most users would expect to be available.
      If you want to provide a custom `cabal-install` package, you'll have to set `haskellTools = lib.mkForce (...)`,
      since the built-in definition doesn't use `mkDefault` to ensure that adding tools in your project won't
      mysteriously remove `cabal-install` from all shells.
      '';
      type = functionTo (listOf package);
      example = literalExpression ''ghc: [ghc.fourmolu]'';
    };

    buildOutputsPrefix = mkOption {
      description = ''
      Some of Hix's features are exposed as top level outputs, like `nix run .#release`.
      Since these are quite numerous, it becomes increasingly likely that these clash with some of the project's package
      or command names, making them inaccessible.

      For this reason, built-in Hix outputs are added to the outputs with lower precedence, to ensure that user-defined
      outputs aren't shadowed.
      To keep the built-in outputs accessible as well, they are additionally exposed in a dedicated prefix set named
      `build`, as in `nix run .#build.release`.

      If you prefer a different prefix, set this option to the desired string.
      '';
      type = str;
      default = "build";
    };

    pkgs = mkOption {
      type = util.types.pkgs;
      description = ''
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

      cabal-extra = mkOption {
        type = attrsOf unspecified;
        default = {};
      };

      hixVersion = mkOption {
        description = "The Hix version used by this project.";
        type = str;
        readOnly = true;
      };

      hixUrl = mkOption {
        description = "The URL to the Hix release tag used by this project.";
        type = str;
        default = "github:tek/hix?ref=${config.internal.hixVersion}";
      };

      removeFlakeLockFromCabalDrvSrc = mkOption {
        description = "If set, flake.lock will be filtered out of the source path for synthetic Cabal derivations.";
        type = types.bool;
        default = false;
      };

    };
  };

  config = {

    packages = mkDefault internal.project.defaultPackages;

    main = mkDefault internal.project.defaultMain;

    name = mkDefault (internal.packages.withMain "hix-project" (pkg: pkg.name));

    pkgs = mkDefault config.envs.dev.ghc.pkgs;

    haskellTools = ghc: [ghc.cabal-install];

    internal = {

      pkgs = import config.inputs.nixpkgs { inherit (config) system; };

      basicGhc = config.internal.pkgs.haskell.packages.${config.compiler};

      packageNames = attrNames config.packages;

      hixVersion = "0.8.0";

    };
  };
}
