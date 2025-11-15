{util}:
{name ? null, config, lib, ...}: let
  inherit (lib) types;
  inherit (util) internal;

  nixpkgsSource = import ./nixpkgs-source.nix { inherit util; };

  options = {

    source = lib.mkOption {
      type = types.oneOf [types.path types.package types.pkgs (types.submodule nixpkgsSource)];
      description = ''
      The path to a nixpkgs source tree, a fully imported nixpkgs set, or a set specifying
      [arguments for a fetcher](#options-nixpkgs-source).

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

      ::: {.note}
      If you specify both [](#opt-nixpkgs-source-url) and [](#opt-nixpkgs-source-rev), the given repo will be cloned,
      which is expensive for Nixpkgs.
      Specifying only [](#opt-nixpkgs-source-rev) will fetch a tarball from the default Github repo instead.
      If you want to fetch a tarball from custom repo, you need to specify a fetcher call manually, like:

      ```
      {
        nixpkgs.default.source = config.pkgs.fetchzip { url = "..."; rev = "..."; hash = "..."; };
      }
      ```
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
