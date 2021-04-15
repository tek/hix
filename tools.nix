{ pkgs }:
with pkgs.lib;
let
  pure = import ./pure.nix;

  overlay = compiler: over: self: super:
  let cabal = import ./cabal-dep.nix { inherit pkgs self super; };
  in pure.overrides over { inherit compiler cabal; } self super;

  composeCabal = lists.foldr (a: z: c: (z c) // a c) (_: {});
  composeOverrides = lists.foldr (a: z: c: composeExtensions (z c) (a c)) pure.noOverrides;

  packageSubpath = base: pp:
  let
    new = strings.removePrefix (toString base) (toString pp);
    success = new == (toString pp);
  in
  if builtins.isPath pp
  then if success then error "invalid package path ${pp}" else new
  else pp;

in {
  inherit overlay composeCabal composeOverrides packageSubpath;
}
