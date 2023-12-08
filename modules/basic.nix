{ config, lib, util, ... }:
with lib;
with types;
let

  global = config;

  packageModule = import ./package.nix { inherit global util; };
  cabalOptionsModule = import ./cabal-options.nix { inherit global util; };

  baseFromPackages = let
    paths = attrValues config.internal.packagePaths;
    pkg = head paths;
    next = p:
      if p == "/"
      then throw "Could not determine project root dir: Invalid package path ${pkg}"
      else if isStorePath p || pathExists "${p}/flake.nix"
      then p
      else next (dirOf p);
  in
   if length paths == 0
   then throw ''
   Could not determine project root dir.
   Either specify `base = ./.;` or add entries to `packages`.
   See https://tryp.io/hix/index.html#packages for more.
   ''
   else next pkg;

   autoMain = let
     ps = attrValues config.packages;

    inDeps = target: thing: any (dep: util.cabalDepPackage dep == target) thing.dependencies;

    hasDepOn = dep: pkg:
    pkg.name != dep &&
    any (inDeps dep) (attrValues pkg.internal.componentsSet);

     isNoDep = pkg: ! (any (hasDepOn pkg.name) ps);

    in findFirst isNoDep null (attrValues config.packages);

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
        If this option is undefined, Hix will choose one of the packages that are not in the dependencies of any other
        package.
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

    # TODO is this effective and desired?
    forceCabalGen = mkOption {
      description = mdDoc ''
      Whether to generate a Cabal file from Nix config even if there is one in the source directory.
      '';
      type = bool;
      default = false;
    };

    ifd = mkOption {
      description = mdDoc ''
      Whether to use `cabal2nix`, which uses Import From Derivation, or to generate simple derivations, for local
      packages.
      '';
      type = bool;
      default = false;
    };

    compiler = mkOption {
      description = mdDoc ''
      The GHC version used for internal tasks and for the default environment.
      This is an attribute name in the nixpkgs set `haskell.packages`, which is usually in the format `ghc96`.
      '';
      type = str;
      default = "ghc94";
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

    haskellTools = mkOption {
      description = mdDoc ''
      Function returning a list of names of Haskell packages that should be included in every environment's `$PATH`.
      This is a convenience variant of [](#opt-env-buildInputs) that provides the environment's GHC package set (without
      overrides) as a function argument.
      This is intended for tooling like `fourmolu`.
      '';
      type = functionTo (listOf package);
      default = _: [];
      example = literalExpression ''ghc: [ghc.fourmolu]'';
    };

    buildOutputsPrefix = mkOption {
      description = mdDoc ''
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

    main = mkDefault (let
      names = config.internal.packageNames;
      count = length names;
    in
      if count == 1
      then head names
      else if count == 0
      then ''
      This action requires at least one package to be defined, as in:
      {
        packages.my-project = { src = ./.; };
      }
      ''
      else if autoMain != null
      then autoMain.name
      else throw ''
      Could not determine the main package.
      This should only happen if all packages depend on each other cyclically.
      If that is not the case, please report a bug at: https://github.com/tek/hix/issues
      You can specify the main package explicitly:
      {
        main = "my-package";
      }
      ''
    );

    pkgs = mkDefault config.envs.dev.ghc.pkgs;

    internal = {

      pkgs = import config.inputs.nixpkgs { inherit (config) system; };

      basicGhc = config.internal.pkgs.haskell.packages.${config.compiler};

      packageNames = attrNames config.packages;

      packagePaths = mapAttrs (_: p: p.src) config.packages;

      relativePackages = util.relativePackages config.base config.internal.packagePaths;

      hixVersion = "0.6.9";

    };
  };
}
