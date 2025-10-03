{util}:
{name ? null, config, lib, ...}: let

  inherit (lib) types;
  inherit (util) internal;

  buildModule = import ./ghc-build.nix { inherit util; };

  compilerWithPackagesModule.options = {

    compiler = lib.mkOption {
      description = ''
      The GHC derivation.
      '';
      type = types.package;
    };

    package-set = lib.mkOption {
      description = ''
      The package set using this compiler.
      '';
      type = util.types.haskellPackages;
    };

  };

  sources = {

    build = lib.mkOption {
      description = ''
      Let Hix build a custom GHC from the configuration in this module.
      '';
      type = types.submodule buildModule;
    };

    overlay = lib.mkOption {
      description = ''
      Insert the compiler and its package set manually by providing a full nixpkgs overlay.
      Requires [](#opt-compiler-tag) to denote the assigned attribute in `haskell.compiler` and `haskell.packages`.
      '';
      type = types.overlay;
    };

    manual = lib.mkOption {
      description = ''
      Like [](#opt-compiler-overlay), but return only the compiler and packages from the overlay function.
      Hix will take care of exposing them at [](#opt-compiler-tag) by calling this function from a full overlay.

      An overlay is function that takes two `pkgs` arguments representing the final and previous state of `pkgs`:
      ```
      {
        source.manual = pkgsFinal: pkgsPrev: {
          compiler = myCompiler;
          packages = myPackageSet;
        };
      }
      ```
      '';
      type = types.functionTo (types.functionTo (types.submodule compilerWithPackagesModule));
    };

  };

  options = {

    nixpkgs = internal.modules.extensibleOption {
      module = util.types.nixpkgs;
      type = util.types.ref.nixpkgs;
      attr = "nixpkgs";
      desc = "from which this GHC will be instantiated";
      extender = config.name;
    };

    # TODO since the default is the global compiler name, it would be more useful if the example was for a custom build.
    source = lib.mkOption {
      type = types.either (types.listOf types.str) (types.either types.str (types.attrTag sources));
      description = ''
      Instructions for obtaining a GHC and its base package set.

      May be specified in three different ways:
      - The name of an attribute in nixpkgs' `haskell.packages`, like `"ghc910"`
      - The path to an attribute in nixpkgs' `haskell.packages`, like `["native-bignum" "ghc910"]`
      - A [submodule config](#options-ghc-build) for a fully customized build (`build`)
      - A nixpkgs overlay that inserts the compiler and its base package set (`overlay`)
      - An overlay function that returns the compiler and its base package set (`manual`)

      The latter two require the tag mentioned in parens to prefix the config:
      ```
      {
        compilers.myghc.source.build = {
          # ...
        };
      }
      ```
      '';
      default = util.config.compiler;
      example = "ghc910";
    };

    tag = lib.mkOption {
      description = ''
      The attribute name at which the compiler and its package set will be inserted into the standard nixpkgs
      structures, `haskell.{compiler,packages}.<tag>`.

      This becomes significant when overriding packages in [a package set](#options-package-set), because overrides may
      influence some of the tooling infrastructure.
      Therefore, the default tag is `hix`, avoiding any name clashes.
      '';
      type = types.str;
      default = "hix";
    };

    customize = util.maybeOption (types.functionTo (types.submodule compilerWithPackagesModule)) {
      description = ''
      Function for transforming the compiler and its base package set after it was constructed by the method configured
      in [](#opt-compiler-source).

      Takes an argument set `{ pkgs, compiler, packages }` and should return `{ compiler, packages }`.
      This can be used, for example, to override the source commit of an existing compiler:
      ```
      {
        source = "ghc9101";
        customize = {pkgs, compiler, packages}: {
          compiler = compiler.override { ghcSrc = pkgs.fetchurl {}; };
          inherit packages;
        };
      }
      ```
      The provided `pkgs` are the final set that includes the overlay by which the compiler is injected.
      '';
    };

  };

in internal.modules.extensibleModule {
  id = "compiler";
  inherit name options config;
  pushDefault = {
    source.build = {};
  };
}
