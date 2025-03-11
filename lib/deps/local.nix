{
  util,
  ifd,
  libraryProfiling,
  profiling,
  localPackage,
  env,
  packages,
}:
let
  inherit (util) config lib;

  cabalDrv = import ../cabal-drv.nix { inherit config lib env; };

  localProfiling = api:
  if profiling
  then api.profiling
  else
  if libraryProfiling
  then api.id
  else api.noprofiling;

  buildInputs = api: opt:
  if lib.isFunction opt
  then opt api.pkgs
  else opt;

  override = api: pkg:
  lib.pipe (localProfiling api) [
    (localPackage api)
    (pkg.override api)
    (api.buildInputs (buildInputs api pkg.buildInputs))
    (api.buildInputs (buildInputs api config.buildInputs))
  ];

  checkIfd = api: name: pkg:
  if ifd
  then api.source.root pkg.src
  else api.drv (cabalDrv.drv api name pkg);

  transformPackage = api: name: pkg: override api pkg;

  mkPackage = api: name: pkg: checkIfd api name pkg;

in [(api: lib.mapAttrs (mkPackage api) packages) (api: lib.mapAttrs (transformPackage api) packages)]
