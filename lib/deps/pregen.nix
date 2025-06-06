{config ? {}}:
{pkgs}: let
  inherit (pkgs) lib;

  spec = import ./spec.nix { inherit (pkgs) lib; };
  deps = import ./default.nix { inherit config; } { inherit pkgs; };

  override = self: super: pkg: specs: let
    data = spec.reifyPregen { inherit pkgs pkg self super; } specs;
  in { ${pkg} = data; };

  overrides = ghc: overrides: let

    result = ghc.override {
      overrides = self: super: let
        os = builtins.removeAttrs (deps.normalize overrides self super) ["__all"];
        decs = lib.concatMapAttrs (override self super) os;
      in decs // { __hix_pkgs = lib.attrNames os; };
    };

    properPackage = _: a: a != null && !(spec.isOC a);

  in lib.filterAttrs properPackage (lib.getAttrs result.__hix_pkgs result);

in {
  inherit override overrides;
}
