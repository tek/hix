{
  config,
  lib,
  localPackage ? _: lib.id,
}:
let
  hixlib = import ../lib/default.nix { inherit lib; };
  gen-cabal = import ../lib/gen-cabal.nix { inherit config lib; };

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
      if config.auto
      then gen-cabal.withCabal name pkg.src
      else noCabalError name;
    fullSrc =
      if wantAuto name pkg
      then autoSrc
      else pkg.src;
  in api.source.root fullSrc;

  checkIfd = api: name: pkg:
  localPackage api (
    if config.ifd
    then checkAuto api name pkg
    else gen-cabal.simpleCabalDrv api name pkg
  );

in api: builtins.mapAttrs (checkIfd api) config.packages
