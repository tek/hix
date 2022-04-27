{
  lib,
}:
let

  deps = import ../deps/default.nix { inherit lib; };

  hixlib = import ./default.nix { inherit lib; };

  ghcWithOverrides = overrides: ghc:
  ghc.override { overrides = deps.compose overrides; };

  foldOverrides = overrideKeys: overrides:
  lib.foldl (z: a: z ++ hixlib.overridesFor overrides a) [] overrideKeys;

  ghcWithNamedOverrides = overrideKeys: overrides:
  ghcWithOverrides (foldOverrides overrideKeys overrides);

in {
  inherit ghcWithOverrides foldOverrides ghcWithNamedOverrides;
}
