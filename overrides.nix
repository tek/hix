{ lib }:
{
  compiler,
  profiling ? true,
  overrides ? {},
  overrideKeys ? ["all" compiler "dev"],
}:
let
  deps = import ./deps/default.nix { inherit lib profiling; };
  hixlib = import ./lib.nix { inherit lib; };
  combinedOverrides = lib.foldl' (z: a: z ++ hixlib.overridesFor overrides a) [] overrideKeys;
in deps.compose combinedOverrides
