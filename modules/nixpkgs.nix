{util}:
{name ? null, config, lib, ...}: let
  inherit (lib) types;
  inherit (util) internal;

  # TODO generalize with ghc-build
  nixpkgsSource.options = {

    url = lib.mkOption {
      description = "URL of a nixpkgs source archive or Git repo.";
      type = types.str;
      default = "https://github.com/nixos/nixpkgs";
    };

    rev = lib.mkOption {
      description = "Commit hash or ref name for the nixpkgs repo.";
      type = types.nullOr types.str;
      default = null;
    };

    hash = lib.mkOption {
      description = "Hash of the nixpkgs source tree.";
      type = types.str;
      default = "";
    };

    args = lib.mkOption {
      description = "Extra arguments for the fetcher (either `fetchurl` or `fetchgit` if `rev` is specified)";
      type = types.attrsOf types.anything;
      default = {};
    };

  };

  options = {

    source = lib.mkOption {
      type = types.oneOf [types.path types.package types.pkgs (types.submodule nixpkgsSource)];
      description = ''
      The path to a nixpkgs source tree, a fully imported nixpkgs set, or a set specifying arguments for a fetcher.

      This can be a flake input or any regular path.

      Defaults to the input named `nixpkgs` of the Hix flake, overridable in user flakes with:
      ```
      {
        inputs.hix.inputs.nixpkgs.url = "...";
      }
      ```

      ::: {.note}
      If a fully imported nixpkgs set is specified, it will be reimported.
      Its original `config` and `overlays` will be merged manually, but the rest of the import arguments will be
      overridden by what's specified in [](#opt-nixpkgs-args).
      :::
      '';
    };

    config = lib.mkOption {
      description = ''
      Configuration options for nixpkgs, corresponding to the toplevel function's arg `config`.

      Defaults to `{ allowUnfree = true; }`.

      This option is not called `config` because that name would interfere with the module system.
      '';
      type = types.attrsOf types.anything;
      default = {};
    };

    overlays = lib.mkOption {
      type = types.listOf util.types.overlay;
      description = "Arbitrary nixpkgs overlays.";
      default = [];
    };

    # TODO this could use the module defined in `pkgs/top-level/config.nix`
    args = lib.mkOption {
      type = types.attrsOf types.anything;
      description = ''
      Arbitrary other arguments for the toplevel nixpkgs function, which will be combined with the
      [config](#opt-nixpkgs-config) and [overlays](#opt-nixpkgs-overlays).
      '';
      default = {};
    };

  };

in internal.modules.extensibleModule {
  id = "nixpkgs";
  attr = "nixpkgs";
  inherit name options config;
  extraConfig = {
    config.allowUnfree = lib.mkDefault true;
    args = { inherit (util.config) system; };
  };
  pushDefault = {
    source = {};
  };
}
