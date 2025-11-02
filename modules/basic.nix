{ config, lib, util, internal, ... }:
let
  inherit (lib) types mkOption literalExpression;

  global = config;

  packageModule = import ./package.nix { inherit global util; };
  cabalOptionsModule = import ./cabal-options.nix { inherit global util; };

in {
  options = let
    inherit (types) str bool;
  in {

    base = mkOption {
      description = ''
      The project's base directory.

      Will be inferred from package source directories if unset.
      If the Hix project is a subdirectory of a git repository and `flake.nix` isn't tracked by git, this option has to
      be set to `./.` explicitly.
      '';
      example = literalExpression "./.";
      type = types.nullOr types.path;
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
      type = types.attrsOf (types.submodule packageModule);
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

    nixpkgs = lib.mkOption {
      description = "Different configurations of nixpkgs.";
      type = types.attrsOf (types.submodule util.types.nixpkgs);
      default = {};
    };

    compilers = lib.mkOption {
      description = "Different GHC builds.";
      type = types.attrsOf (types.submodule util.types.compiler);
      default = {};
    };

    package-sets = lib.mkOption {
      description = "Different GHC package set configurations.";
      type = types.attrsOf (types.submodule util.types.package-set);
      default = {};
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
      type = types.deferredModule;
      default = {};
    };

    cabal-config = mkOption {
      description = ''
      Evaluated version of {option}`cabal`, for referencing in other config values.
      May not be set by the user.
      '';
      type = types.submoduleWith { modules = [cabalOptionsModule config.cabal config.internal.cabal-extra]; };
      readOnly = true;
      default = {};
    };

    ifd = mkOption {
      description = ''
      Whether to use `cabal2nix`, which uses Import From Derivation, or to generate simple derivations, for local
      packages.
      '';
      type = bool;
      default = false;
    };

    genCabalInDerivations = lib.mkOption {
      description = ''
      Whether [Hix-generated derivations](#no-ifd) should synthesize Cabal files from [](#opt-general-packages).
      '';
      type = types.bool;
      default = true;
    };

    # TODO cli must be able to resolve components from metadata by reading cabal.project and *.cabal
    manualCabal = mkOption {
      description = ''
      Don't use the options in [](#opt-general-packages) as Cabal configuration for the ghci preprocessor and search
      path assembler.
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
      default = "ghc910";
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
        type = types.listOf str;
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
      type = types.listOf types.path;
      default = [];
      description = ''
        Flake inputs containing hix projects whose overrides are merged into this project's.
        The `local` overrides are ignored to prevent the dependencies' project packages from being
        injected into the compat checks.
      '';
    };

    depsFull = mkOption {
      type = types.listOf types.path;
      default = [];
      description = ''
        Flake inputs containing hix projects whose overrides are merged into this project's.
        Unlike `deps`, this includes the `local` overrides.
      '';
    };

    shellTools = mkOption {
      description = ''
      Packages that should be included in every environment's `$PATH`.
      The argument passed to the function is the environment's nixpkgs set.

      If you want to include a tool that's not specific to the environment, you can also ignore the argument and use the
      [global pkgs](opt-general-pkgs) or any other source of packages.

      The default consists of `cabal-install` (from [](#opt-general-build-tools.cabal.package)), since that's a crucial
      tool most users would expect to be available.
      If you want to provide a custom `cabal-install` package for environments only (rather than overriding
      [](#opt-general-build-tools.cabal.package)), you'll have to set `shellTools = lib.mkForce (...)`, since the
      built-in definition doesn't use `mkDefault` to ensure that adding tools in your project won't mysteriously remove
      `cabal-install` from all shells.
      '';
      type = types.functionTo (types.listOf types.package);
      default = _: [];
    };

    haskellTools = mkOption {
      description = ''
      Haskell packages that should be included in every environment's `$PATH`.
      This is a convenience variant of [](#opt-env-buildInputs) that provides the environment's GHC package set (without
      overrides) as a function argument.
      This is intended for tooling like `fourmolu`.
      '';
      type = types.functionTo (types.listOf types.package);
      default = _: [];
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
      type = types.pkgs;
      description = ''
      The vanilla nixpkgs set configured by [`nixpkgs.internal`](#opt-general-nixpkgs).
      Convenience read-only option for the purpose of generic functionality like `pkgs.writeText`.

      Don't use this if you're customizing specific package sets and accessing Haskell packages, but the corresponding
      set configured by [](#options-package-set) instead.
      In particular, [override](#overrides-combinators) functions are provided with the proper `pkgs` argument.
      '';
      readOnly = true;
    };

    internal = {

      pkgs = internal.modules.deprecatedOption {
        type = types.unspecified;
        key = "internal.pkgs";
      };

      basicGhc = internal.modules.deprecatedOption {
        type = util.types.haskellPackages;
        key = "internal.basicGhc";
      };

      packageNames = internal.modules.deprecatedOption {
        type = types.listOf str;
        key = "internal.packageNames";
      };

      cabal-extra = mkOption {
        type = types.attrsOf types.unspecified;
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

    packages = lib.mkDefault internal.project.defaultPackages;

    main = lib.mkDefault internal.project.defaultMain;

    name = lib.mkDefault (internal.packages.withMain "hix-project" (pkg: pkg.name));

    nixpkgs = {

      default = {
        extends = null;
        source = lib.mkDefault util.config.inputs.nixpkgs;
        config.allowUnfree = lib.mkDefault true;
        args = { inherit (util.config) system; };
      };

      internal = {
        extends = null;
        source = lib.mkDefault util.config.inputs.nixpkgs;
        config.allowUnfree = lib.mkDefault true;
        args = { inherit (util.config) system; };
      };

    };

    compilers.default = {
      extends = null;
    };

    package-sets.default = {
      extends = null;
    };

    pkgs = lib.mkDefault util.pkgs;

    shellTools = _: [config.build-tools.cabal.package];

    internal = {

      pkgs = import config.inputs.nixpkgs { inherit (config) system; };

      basicGhc = config.pkgs.haskell.packages.${config.compiler};

      packageNames = lib.attrNames config.packages;

      hixVersion = "0.9.1";

    };
  };
}
