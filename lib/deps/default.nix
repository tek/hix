{ pkgs, }:
with pkgs.lib;
let
  depspec = import ./spec.nix { inherit (pkgs) lib; };

  package = self: super: pkg: spec:
  let
    final = depspec.call spec { inherit pkgs self super pkg; } depspec.initial;
    drv = if final.drv == null then super.${pkg} else final.drv;
  in final.transform drv;

  packages = overlay: self: super:
  mapAttrs (package self super) (overlay (import ./api.nix { inherit pkgs self super; }));

  foldExtensions =
  foldr composeExtensions (self: super: {});

  compose = overlays: foldExtensions (map packages overlays);
in {
  inherit packages compose;
}
