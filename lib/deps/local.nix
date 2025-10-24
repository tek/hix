{
  util,
  ifd,
  libraryProfiling,
  profiling,
  localPackage,
  envName,
  packages,
  buildInputs,
}:
let
  inherit (util) config lib;

  cabalDrv = import ../cabal-drv.nix { inherit config lib envName; };

  localProfiling = api:
  if profiling
  then api.profiling
  else
  if libraryProfiling
  then api.id
  else api.noprofiling;

  override = api: pkg:
  lib.pipe (localProfiling api) [
    (localPackage api)
    (pkg.override api)
    (api.buildInputs (pkg.buildInputs api.pkgs))
    (api.buildInputs (buildInputs api.pkgs))
    (api.buildInputs (config.buildInputs api.pkgs))
  ];

  checkIfd = api: name: pkg:
  if ifd
  then api.source.root pkg.src
  else api.drv (cabalDrv.drv api name pkg);

  transformPackage = api: name: pkg: override api pkg;

  mkPackage = api: name: pkg: checkIfd api name pkg;

in [(api: lib.mapAttrs (mkPackage api) packages) (api: lib.mapAttrs (transformPackage api) packages)]
