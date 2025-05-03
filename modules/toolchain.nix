{util}:
{config, ...}: let

  inherit (util) lib;
  inherit (lib) types;

in {

  options = {

    pkgs = lib.mkOption {
      description = ''
      Instantiated nixpkgs including the GHC overlay.
      '';
      type = types.pkgs;
    };

    compiler = lib.mkOption {
      description = ''
      The compiler package.
      '';
      type = types.package;
    };

    packages = lib.mkOption {
      description = ''
      The package set with overrides.
      '';
      type = util.types.haskellPackages;
    };

    vanilla = lib.mkOption {
      description = ''
      The package set without overrides.
      '';
      type = util.types.haskellPackages;
    };

    overrides = lib.mkOption {
      description = ''
      The overrides used by [](#opt-toolchain-packages), for reference.
      '';
      type = util.types.cabalOverridesVia "toolchain";
      readOnly = true;
    };

    version = lib.mkOption {
      description = ''
      The version of GHC in [](#opt-toolchain-compiler).
      '';
      type = types.str;
      readOnly = true;
    };

    tag = lib.mkOption {
      description = ''
      The attribute name that the compiler and package set use in `haskell.{compiler,packages}`.
      '';
      type = types.str;
      readOnly = true;
    };

    conf = lib.mkOption {
      description = ''
      The configuration of the package set used to build this toolchain.
      '';
      type = types.raw;
      readOnly = true;
      default = {};
    };

  };

}
