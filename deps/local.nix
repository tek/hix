{
  config,
  lib,
  localPackage ? _: lib.id,
}:
let
  hixlib = import ../lib/default.nix { inherit lib; };
  gen-cabal = import ../lib/gen-cabal.nix { inherit config lib; };

  local = api: name: src:
  (localPackage api) (
    if config.ifd
    then api.source.root src else
    if lib.hasAttr name config.hpack.packages
    then gen-cabal.simpleCabalDrv api name src
    else gen-cabal.inferCabalDrv api name src
  );

  noCabal = name: p:
  !(builtins.pathExists "${p}/${name}.cabal" || builtins.pathExists "${p}/package.yaml");

  genAuto = api: name: p:
  local api name (gen-cabal.withCabal name p);

  withAuto = api: name: p:
  if config.forceCabal || noCabal name p
  then
    if config.auto
    then genAuto api name p
    else throw ''
      Can't build package '${name}' since no Cabal or HPack file is present in the source directory.
      Set 'auto = true' to generate config on the fly.
      If the file exists, you might have to 'git add' it or its name isn't '${name}.cabal'.
    ''
  else local api name p;

in api: builtins.mapAttrs (withAuto api) config.packages
