{
  lib,
}:
with builtins;
with lib;
let
  depspec = import ./spec.nix { inherit lib; };

  package = self: super: pkg: spec:
  let
    pkgs = self.callPackage ({ pkgs, ... }: pkgs) {};
    final = depspec.call spec { inherit pkgs self super pkg; } depspec.initial;
    drv = if final.drv == null then super.${pkg} else final.drv;
  in final.transform drv;

  packages = overlay: self: super:
  let
    pkgs = self.callPackage ({ pkgs, ... }: pkgs) {};
    api = import ./api.nix { inherit pkgs; };
  in mapAttrs (package self super) (overlay (api { inherit self super; }));

  foldExtensions =
  foldr composeExtensions (self: super: {});

  compose = overlays: foldExtensions (map packages overlays);

  override = ghc: f: ghc.override { overrides = compose f; };
in {
  inherit packages compose override;
}
