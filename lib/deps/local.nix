{
  config,
  lib,
  ifd,
  profiling,
  localPackage,
}:
with lib;
let
  cabalDrv = import ../cabal-drv.nix { inherit config lib; };

  localProfiling = api: if profiling then id else api.noprofiling;

  buildInputs = api: opt:
  if isFunction opt
  then opt api.pkgs
  else opt;

  override = api: pkg: drv:
  api.buildInputs (buildInputs api config.buildInputs)
  (api.buildInputs (buildInputs api pkg.buildInputs) (pkg.override api (localPackage api (localProfiling api drv))));

  checkIfd = api: name: pkg:
  if ifd
  then api.source.root pkg.src
  else api.drv (cabalDrv.drv api name pkg);

  mkPackage = api: name: pkg: override api pkg (checkIfd api name pkg);

in api: builtins.mapAttrs (mkPackage api) config.packages
