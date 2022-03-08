{ config, lib, normalizeOverrides, relativePackages, ... }:
with lib;
with types;
let

  global = config;

  ghcModule = import ./ghc.nix global;

  compatProject = { name, config, ... }: {
    options = {
      name = mkOption {
        type = str;
        default = name;
      };

      prefix = mkOption {
        type = str;
      };

      version = mkOption {
        type = str;
        default = "ghc${name}";
      };

      ghc = mkOption {
        type = submodule ghcModule;
      };
    };

    config = {
      prefix = mkDefault "compat-${config.name}";
      ghc = {
        compiler = config.version;
        overrideKeys = ["local" "all" "compat" config.ghc.compiler];
        nixpkgs = global.input.ghcNixpkgs."${config.ghc.compiler}" or global.inputs.nixpkgs;
      };
    };
  };

  compatProjects = {
    "921" = {};
    "902" = {};
    "8107" = {};
    "884" = {};
  };

  overrides =
  let
    local = import ../deps/local.nix {
      inherit lib;
      inherit (config) base packages localPackage;
    };
    localMin = import ../deps/local.nix {
      inherit lib;
      inherit (config) base packages;
      localPackage = { fast, ... }: p: fast (config.localPackage p);
    };
    withDeps = normalizeOverrides config.overrides config.deps;
  in withDeps // { all = (withDeps.local or []) ++ withDeps.all; local = [local]; localMin = [localMin]; };

in {
  options = {

    base = mkOption {
      type = path;
      description = "The project's base directory.";
      example = literalExpression "./.";
    };

    packages = mkOption {
      type = attrsOf path;
      description = ''
        The project's Cabal packages, with keys being the Cabal names and values pointing to the directory containing
        the Cabal file.
      '';
      example = literalExpression ''
      {
        spaceship = ./.;
        spaceship-api = ./api;
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

    mainCompiler = mkOption {
      type = str;
      default = "ghc902";
      description = ''
        The GHC version used for internal tasks and as default for the dev package set.
      '';
    };

    overrides = mkOption {
      type = unspecified;
      default = {};
      # TODO link combinators doc
      description = ''
        Cabal package specifications and overrides injected into GHC package sets.
        Each override spec is a list of dep functions, which are called with a set of combinators and resources like
        nixpkgs and should return an attrset containing either derivations or a transformation built from those
        combinators.
        The combinators are described at TODO.
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
        ghc921 = [{ source, minimal, ... }: {
          lens = minimal (source.root inputs.lens);
        }];
      }
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
      type = unspecified;
      # TODO this appears to work in tests, verify
      # type = listOf path;
      default = [];
      description = "Flake inputs containing hix projects whose overrides are merged into this project's.";
    };

    devGhc = mkOption {
      type = submodule ghcModule;
      description = ''
      The GHC package set with overrides that is used primarily, like when building the default package with
      <literal>nix build</literal> or running shells.
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
        type = unspecified;
        description = ''
          Internal option that computes the full overrides, combining <literal>overrides</literal> with the full
          overrides from <literal>deps</literal> and the local package derivations.
        '';
      };

      basicPkgs = mkOption {
        type = unspecified;
      };

      basicGhc = mkOption {
        type = unspecified;
      };

      relativePackages = mkOption {
        type = attrsOf str;
      };
    };
  };

  config = {
    main = mkIf (length (attrNames config.packages) == 1) (mkDefault (head (attrNames config.packages)));

    internal.basicPkgs = import config.inputs."nixpkgs_${config.mainCompiler}" { inherit (config) system; };

    internal.basicGhc = config.internal.basicPkgs.haskell.packages.${config.mainCompiler};

    devGhc = mkDefault {};

    minDevGhc = mkDefault (config.devGhc // {
      overrideKeys = ["localMin" "all" config.minDevGhc.compiler "dev"];
    });

    compat.projects = mkDefault compatProjects;

    internal = {
      overrides = mkDefault overrides;

      relativePackages = mkDefault (relativePackages config.base config.packages);
    };
  };
}
