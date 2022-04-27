{
  compiler,
  overrides ? {},
  overrideKeys ? ["local" "all" compiler "dev"],
}:
final: prev:
let
  o = import ./overrides.nix { inherit (prev) lib; };
in {
  hixPackages = o.ghcWithNamedOverrides overrideKeys overrides prev.haskell.packages.${compiler};
}
