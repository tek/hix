{
  pkgs,
  profiling ? true,
}:
with builtins;
with pkgs.lib;
let
  depspec = import ./spec.nix { inherit (pkgs) lib; };
  api = import ./api.nix { inherit pkgs profiling; };
  tools = import ./cabal-spec-tools2.nix { inherit pkgs profiling; };

  package = self: super: pkg: spec:
  let
    final = depspec.call spec { inherit pkgs self super pkg; } depspec.initial;
    drv = if final.drv == null then super.${pkg} else final.drv;
  in final.transform drv;

  packages = overlay: self: super:
  mapAttrs (package self super) (overlay (api { inherit self super; }));

  asList = overlays:
  if isList overlays then overlays else [overlays];

  composeManyExtensions =
    foldr composeExtensions (self: super: {});

  compose = overlays: composeManyExtensions (map packages (asList overlays));

  override = ghc: f: ghc.override { overrides = compose f; };
in {
  inherit packages compose tools override;
}
