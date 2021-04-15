{ pkgs }:
with pkgs.lib.lists;
let
  pure = import ./pure.nix;

  overlay = compiler: over: self: super:
  let cabal = import ./cabal-dep.nix { inherit pkgs self super; };
  in pure.overrides over { inherit compiler cabal; } self super;

  composeCabal = foldr (a: z: c: (z c) // a c) (_: {});
  composeOverrides = foldr (a: z: c: pkgs.lib.composeExtensions (z c) (a c)) pure.noOverrides;
in {
  inherit overlay composeCabal composeOverrides;
}
