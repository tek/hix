{
  config,
  lib,
  ifd,
  libraryProfiling,
  profiling,
  localPackage,
  env,
}:
let
  cabalDrv = import ../cabal-drv.nix { inherit config lib env; };

  localProfiling = api:
  if profiling
  then api.profiling
  else
  if libraryProfiling
  then lib.id
  else api.noprofiling;

  buildInputs = api: opt:
  if lib.isFunction opt
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

in api: lib.mapAttrs (mkPackage api) config.packages
