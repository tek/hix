{
  config,
  lib,
  ifd,
  auto,
  localPackage ? _: lib.id,
}:
with lib;
let
  gen-cabal = import ../gen-cabal.nix { inherit config lib; };

  noCabal = name: src:
  !(builtins.pathExists "${src}/${name}.cabal" || builtins.pathExists "${src}/package.yaml");

  noCabalError = name:
    throw ''
    Can't build package '${name}' since no Cabal or HPack file is present in the source directory.
    Set 'auto = true' to generate config on the fly.
    If the file exists, you might have to 'git add' it or its name isn't '${name}.cabal'.
    '';

  wantAuto = name: pkg:
  !config.forceCabal2nix && (config.forceCabalGen || noCabal name pkg.src);

  checkAuto = api: name: pkg:
  let
    autoSrc =
      if auto
      then gen-cabal.withCabal name pkg.src
      else noCabalError name;
    fullSrc =
      if wantAuto name pkg
      then autoSrc
      else pkg.src;
  in api.source.root fullSrc;

  buildInputs = api: opt:
  if isFunction opt
  then opt api.pkgs
  else opt;

  override = api: pkg: drv:
  api.buildInputs (buildInputs api config.buildInputs)
  (api.buildInputs (buildInputs api pkg.buildInputs) (pkg.override api (localPackage api drv)));

  checkIfd = api: name: pkg:
  if ifd
  then checkAuto api name pkg
  else api.drv (gen-cabal.simpleCabalDrv api name pkg);

  mkPackage = api: name: pkg: override api pkg (checkIfd api name pkg);

in api: builtins.mapAttrs (mkPackage api) config.packages
