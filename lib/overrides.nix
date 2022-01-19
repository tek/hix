{ lib }:
{
  compiler,
  profiling ? true,
  overrides ? {},
  overrideKeys ? ["all" compiler "dev"],
}:
let
  deps = import ../deps/default.nix { inherit lib profiling; };
  hixlib = import ./default.nix { inherit lib; };
  combinedOverrides = lib.foldl (z: a: z ++ hixlib.overridesFor overrides a) [] overrideKeys;
in deps.compose combinedOverrides
