{ config, lib, util, ... }:
with lib;
with types;
let

  global = config;

  ghcModule = import ./ghc.nix { inherit global util; };
  packageModule = import ./package.nix { inherit global util; };
  cabalOptionsModule = import ./cabal-options.nix { inherit util; };

  compatProject = { name, config, ... }: {
    options = {
      enable = mkOption {
        type = bool;
        description = "Whether this version should be included.";
        default = true;
      };

      name = mkOption {
        type = str;
        description = "The name is used in the default prefix as <literal>compat-{name}</literal>.";
        default = name;
      };

      prefix = mkOption {
        type = str;
        description = ''
        All compat check derivations are prefixed with this, so you would run:
         <literal>nix build .#{prefix}-{package}</literal>.
        '';
      };

      version = mkOption {
        type = str;
        default = "ghc${name}";
        description = ''
          The attribute name for a GHC version in the set <literal>haskell.packages</literal> used for this compat
          project.
        '';
      };

      ghc = mkOption {
        type = submodule ghcModule;
        description = ''
        The GHC config used for this compat project.
        The default uses <literal>version</literal> for the <literal>compiler</literal> option, adds the version to
        <literal>overrideKeys</literal> and uses the nixpkgs input named <literal>nixpkgs_{version}</literal>.
        '';
      };
    };

    config = {
      prefix = mkDefault "compat-${config.name}";
      ghc = {
        name = config.name;
        compiler = config.version;
        overrideKeys = ["local" "all" "compat" config.ghc.compiler];
        nixpkgs = global.input.ghcNixpkgs."${config.ghc.compiler}" or global.inputs.nixpkgs;
      };
    };
  };

  compatProjects = {
    "943" = {};
    "925" = {};
    "902" = {};
    "8107" = { enable = false; };
    "884" = { enable = false; };
  };

  overrides =
  let
    local = import ../deps/local.nix {
      inherit config lib;
      inherit (config) localPackage;
    };
    localMin = import ../deps/local.nix {
      inherit config lib;
      localPackage = api@{ fast, ... }: p: fast (config.localPackage api p);
    };
    withDeps = util.normalizeOverrides config.overrides config.deps config.depsFull;
  in withDeps // { all = (withDeps.local or []) ++ withDeps.all; local = [local]; localMin = [localMin]; };

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
      description = "The project's base directory.";
      example = literalExpression "./.";
    };

    packages = mkOption {
      type = attrsOf (submodule packageModule);
      description = ''
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
    };

    main = mkOption {
      type = str;
      description = ''
        The name of a key in <literal>packages</literal> that is considered to be the main package.
        This package will be assigned to the <literal>defaultPackage</literal> flake output that is built by a plain
        <literal>nix build</literal>.
      '';
    };

    cabal = mkOption {
      type = unspecified;
      description = ''
      Cabal options that are applied to all packages and components.

      If you define any options here, they will be merged with definitions that are set in packages or components.
      This means that the default priority handling applies – definitions in components don't automatically override
      those in packages or the global config.
      You will need to use <literal>mkDefault</literal> or <literal>mkForce</literal>, or even
      <literal>mkOverride</literal> if you define an option at all three levels.

      **Note**: In order to enable cascading of these options, the definitions are not evaluated in-place, but when
      evaluating packages and components. Therefore, referring to these values with e.g.
      <literal>config.cabal.version</literal> does not work as expected if the value uses an option property like
      <literal>mkIf</literal> or <literal>mkOverride</literal>.
      You can use {option}`cabal-config` for this purpose, though.
      '';
      default = {};
    };

    # TODO use readOnly for other instances of this
    cabal-config = mkOption {
      type = submoduleWith { modules = [config.cabal cabalOptionsModule]; };
      readOnly = true;
      description = ''
      Evaluated version of {option}`cabal`, for referencing in other config values.
      May not be set by the user.
      '';
      default = {};
    };

    auto = mkOption {
      type = bool;
      default = false;
      description = ''
        Generate the Cabal file on the fly if none is present in the source directory (or a
        <literal>package.yaml</literal>).
      '';
    };

    forceCabal2nix = mkOption {
      type = bool;
      default = false;
      description = "Whether to use cabal2nix even if there is no Cabal file.";
    };

    forceCabalGen = mkOption {
      type = bool;
      default = false;
      description = "Whether to generate a Cabal file from Nix config even if there is one in the source directory.";
    };

    ifd = mkOption {
      type = bool;
      default = true;
      description = "Whether to use cabal2nix, which uses Import From Derivation, or to generate simple derivations.";
    };

    mainCompiler = mkOption {
      type = str;
      default = "ghc925";
      description = ''
        The GHC version used for internal tasks and as default for the dev package set.
      '';
    };

    overrides = mkOption {
      type = util.types.cabalOverrides;
      default = {};
      description = ''
        Cabal package specifications and overrides injected into GHC package sets.
        Each override spec is a list of dep functions, which are called with a set of combinators and resources like
        nixpkgs and should return an attrset containing either derivations or a transformation built from those
        combinators.
        The combinators are described in
        <link xlink:href="https://github.com/tek/hix#built-in-depspec-combinators">readme.md</link>.
        If this is given as a single deps function or a list thereof, it will be converted to
        <literal>{ all = [o]; dev = [o]; }</literal> (without the brackets for a list).
        The keys are used to select the override functions requested by a package set's <literal>overrideKeys</literal>
        option, with identical package names in later entries overriding earlier ones (cumulatively, like the earlier
        ones being in <literal>super</literal>).
      '';
      example = literalExpression ''
      {
        all = [{ hackage, fast, jailbreak, ... }: {
          aeson = fast (hackage "2.0.0.0" "sha54321");
          http-client = unbreak;
        }];
        ghc925 = [{ source, minimal, ... }: {
          lens = minimal (source.root inputs.lens);
        }];
      }
      '';
    };

    extraOverrides = mkOption {
      type = lazyAttrsOf (listOf unspecified);
      default = {};
      description = ''
        Like <literal>overrides</literal>, but expected to be in normalized form. This allows for extensions of Hix to
        add overrides from multiple locations, since the <literal>listOf</literal> aggregates all definitions.
      '';
    };

    localPackage = mkOption {
      type = unspecified;
      default = _: id;
      description = ''
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
      description = ''
        Global default for whether to build local packages and dependency overrides with profiling enabled.
      '';
    };

    compat = {
      enable = mkOption {
        type = bool;
        default = true;
        description = ''
          Create derivations in <literal>outputs.checks</literal> that build the packages with different GHC versions.
          The set of versions is configured by <literal>compat.versions</literal>
        '';
      };

      projects = mkOption {
        type = attrsOf (submodule compatProject);
        description = ''
          The set of GHC versions for which additional checks should be generated, numbers corresponding to the suffix
          of the package set, as in <literal>pkgs.haskell.packages.ghc902</literal>.
          The values configure the package set used for this version.
        '';
      };
    };

    deps = mkOption {
      type = listOf path;
      default = [];
      description = ''
        Flake inputs containing hix projects whose overrides are merged into this project's.
        The <literal>local</literal> overrides are ignored to prevent the dependencies' project packages from being
        injected into the compat checks.
      '';
    };

    depsFull = mkOption {
      type = listOf path;
      default = [];
      description = ''
        Flake inputs containing hix projects whose overrides are merged into this project's.
        Unlike <literal>deps</literal>, this includes the <literal>local</literal> overrides.
      '';
    };

    depsProf = mkOption {
      type = listOf path;
      default = [];
      description = ''
        Flake inputs containing hix projects whose overrides are merged into this project's.
        Unlike <literal>deps</literal>, this includes the <literal>local</literal> overrides.
        Unlike <literal>depsFull</literal>, the local packages are forced to be built with profiling enabled.
      '';
    };

    devGhc = mkOption {
      type = submodule ghcModule;
      description = ''
        The GHC package set with overrides that is used primarily, like when building the default package with
        <literal>nix build</literal> or running shells.
      '';
    };

    pkgs = mkOption {
      type = util.types.pkgs;
      description = ''
        The nixpkgs attrset used by <literal>devGhc</literal>.
      '';
    };

    minDevGhc = mkOption {
      type = submodule ghcModule;
      description = ''
        A copy of <literal>devGhc</literal> in which the derivations of the local packages have some features disabled
        (haddock, profiling) to speed up compilation.
      '';
    };

    internal = {

      overrides = mkOption {
        type = attrsOf (listOf unspecified);
        description = ''
          Internal option that computes the full overrides, combining <literal>overrides</literal> with the full
          overrides from <literal>deps</literal> and the local package derivations.
        '';
      };

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
          description = "The GHC config used for the Hix CLI, defaulting to the dev GHC without overrides.";
          type = submodule ghcModule;
        };

        overrides = mkOption {
          type = util.types.cabalOverrides;
          description = "The overrides used for the CLI client.";
        };

        package = mkOption {
          description =
            "The package for the Hix CLI, defaulting to the local package in the input repository using the dev GHC.";
          type = package;
        };

      };

    };
  };

  config = {
    base = mkDefault baseFromPackages;

    main = mkDefault (
      if (length config.internal.packageNames == 1)
      then head config.internal.packageNames
      else throw "The config option 'main' must name one of the 'packages' if more than one is defined."
    );

    devGhc = { name = "dev"; };

    pkgs = mkDefault config.devGhc.pkgs;

    minDevGhc = {
      name = "min";
      compiler = config.devGhc.compiler;
      nixpkgs = config.devGhc.nixpkgs;
      nixpkgsOptions = config.devGhc.nixpkgsOptions;
      overrideKeys = ["localMin" "all" config.minDevGhc.compiler "dev"];
      overlays = config.devGhc.overlays;
    };

    compat.projects = compatProjects;

    internal = {

      basicPkgs = import config.inputs."nixpkgs_${config.mainCompiler}" { inherit (config) system; };

      basicGhc = config.internal.basicPkgs.haskell.packages.${config.mainCompiler};

      overrides = util.mergeOverrides [config.extraOverrides overrides];

      packageNames = attrNames config.packages;

      packagePaths = mapAttrs (_: p: p.src) config.packages;

      relativePackages = util.relativePackages config.base config.internal.packagePaths;

      hixCli = let
        cfg = config.internal.hixCli;
      in {

        overrides = mkDefault {
          hix = {hackage, source, ...}: {
            exon = hackage "1.4.0.0" "1m4i3a14wip985ncblfy2ikcy7gw5rryj9z497ah218d1nmwj7rl";
            flatparse = hackage "0.4.0.2" "0saxwgwbzijgm9v5w9nx3npl28szpkyz97m4shn8yanxq7gsjnvg";
            incipit-base = hackage "0.5.0.0" "02fdppamn00m94xqi4zhm6sl1ndg6lhn24m74w24pq84h44mynl6";
            hix = source.root ../packages/hix;
          };
        };

        ghc = {
          name = "hix";
          compiler = config.devGhc.compiler;
          nixpkgs = config.devGhc.nixpkgs;
          nixpkgsOptions = config.devGhc.nixpkgsOptions;
          overrides = cfg.overrides;
          overrideKeys = ["hix"];
          overlays = config.devGhc.overlays;
        };

        package = cfg.ghc.ghc.hix;

      };

    };
  };
}
