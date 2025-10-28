{util}:
{name ? null, lib, config, ...}: let

  inherit (lib) types;
  inherit (util) internal;

  options = {

    compiler = internal.modules.extensibleOption {
      module = util.types.compiler;
      type = util.types.ref.compiler;
      attr = "compilers";
      desc = "that provides the GHC used to build this package set";
      extender = config.name;
    };

    overrides = lib.mkOption {
      type = util.types.cabalOverrides;
      description = ''
      The overrides used for this package set – see [](#ghc) for an explanation.
      '';
      default = [];
    };

    extraOverrides = lib.mkOption {
      type = util.types.cabalOverrides;
      description = ''
      Like [](#opt-package-set-overrides), but intended to be set from environments or other consumers of package sets.
      '';
      default = [];
    };

    cross = util.maybeOption types.str {
      description = ''
      The name of a cross architecture supported by nixpkgs, consisting of the attributes in `pkgsCross`, which are
      defined in `lib/systems/examples.nix` (Consult the [manual](https://nixos.org/manual/nixpkgs/unstable/#chap-cross)
      for details).
      For more flexible configuration, you can customize the [nixpkgs](#options-nixpkgs) used for this set.
      '';
      example = "musl64";
    };

    gen-overrides = lib.mkOption {
      description = ''
      Allow this package set to use pregenerated overrides.
      Has no effect when [](#opt-general-gen-overrides.enable) is `false`.

      Disabled by default, but enabled for package sets that are defined in an environment.
      '';
      type = types.bool;
      default = false;
    };

    customize = util.maybeOption (types.functionTo util.types.haskellPackages) {
      description = ''
      Function for transforming the package set after it was constructed from [the compiler](#opt-package-set-compiler).

      Should takes an argument set `{ pkgs, packages }` and return the modified `packages`.
      The provided `pkgs` are the final set that includes the overlay by which the package set is injected.
      '';
    };

  };

in internal.modules.extensibleModule {
  id = "package-set";
  inherit name options config;
}
