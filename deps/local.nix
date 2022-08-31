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

  wantAuto = name: src:
  !config.forceCabal2nix && (config.forceCabalGen || noCabal name src);

  checkAuto = api: name: src:
  let
    autoSrc =
      if config.auto
      then gen-cabal.withCabal name src
      else noCabalError name;
    fullSrc =
      if wantAuto name src
      then autoSrc
      else src;
  in api.source.root fullSrc;

  checkIfd = api: name: src:
  localPackage api (
    if config.ifd
    then checkAuto api name src
    else gen-cabal.simpleCabalDrv api name src
  );

in api: builtins.mapAttrs (checkIfd api) config.packages
