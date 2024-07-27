{ config, lib, ... }:
with builtins;
with lib;
let

  hpackFile = conf:
  toFile "package.yaml" (toJSON (removeAttrs conf ["passthru"]));

  withoutLock = name: srcPath: builtins.path {
    path = srcPath;
    filter = path: type: type != "regular" || baseNameOf path != "flake.lock";
    name = "${name}-cabal-drv-src";
  };

  srcWithCabal = pkgs: conf: name: srcPath: let
    src = if config.internal.removeFlakeLockFromCabalDrvSrc then withoutLock name srcPath else srcPath;
    cabal = "${name}.cabal";
  in pkgs.runCommand "${name}-cabal-drv" {} ''
  cp -r ${src} $out
  chmod u+w $out
  cd $out
  rm -f package.yaml
  cp ${hpackFile conf} package.yaml
  if [[ -e ${cabal} ]]
  then
    chmod u+w ${cabal}
  fi
  ${config.internal.basicGhc.hpack}/bin/hpack --force
  rm -f package.yaml
  '';

  depPkg = spec: let
    name =
    if isAttrs spec
    then spec.name
    else spec;
    in head (splitString ":" (head (splitString " " name)));

  drvWith = conf: { pkgs, self, hsLib, ... }: pname: pkg:
  let
    attr = n:
    if hasAttr n conf
    then conf.${n}
    else throw "The Cabal config for '${pname}' is missing the mandatory attribute '${n}'.";

    dep = n:
    if hasAttr n self
    then self.${n}
    else throw "The Cabal config for '${pname}' has a dependency on the nonexistent package '${n}'.";

    depspec = spec: let
      name = depPkg spec;
    in
    if name == pname
    then []
    else [(dep name)];

    deps = c: concatMap depspec (c.dependencies or []);

  in self.callPackage ({mkDerivation}: mkDerivation ({
    inherit pname;
    src = srcWithCabal pkgs conf pname pkg.src;
    version = attr "version";
    license = attr "license";
    libraryHaskellDepends = deps (conf.library or {});
    executableHaskellDepends = concatMap deps (attrValues (conf.executables or {}));
    testHaskellDepends = concatMap deps (attrValues (conf.tests or {}));
    benchmarkHaskellDepends = concatMap deps (attrValues (conf.benchmarks or {}));
  } // conf.passthru or {})) {};

  drv = api: pname:
  drvWith config.hpack.internal.packages.${pname} api pname;

in {
  inherit drv;
}
