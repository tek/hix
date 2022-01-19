{
  lib,
  base,
  packages,
  localPackage ? _: lib.id,
}:
let
  hixlib = import ../lib/default.nix { inherit lib; };

  local = api: p:
  (localPackage api) (api.source.root (hixlib.packagePath base p));

in api: builtins.mapAttrs (_: local api) packages
