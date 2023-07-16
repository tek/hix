{
  config,
  lib,
  ifd,
  localPackage ? _: lib.id,
}:
with lib;
let
  gen-cabal = import ../gen-cabal.nix { inherit config lib; };
  c2n = import ./cabal2nix.nix { inherit (config) pkgs; };

  buildInputs = api: opt:
  if isFunction opt
  then opt api.pkgs
  else opt;

  override = api: pkg: drv:
  api.buildInputs (buildInputs api config.buildInputs)
  (api.buildInputs (buildInputs api pkg.buildInputs) (pkg.override api (localPackage api drv)));

  checkIfd = api: name: pkg:
  if ifd
  then api.source.root pkg.src
  else api.drv (gen-cabal.simpleCabalDrv api name pkg);

  mkPackage = api: name: pkg: override api pkg (checkIfd api name pkg);

in api: builtins.mapAttrs (mkPackage api) config.packages
