{config, lib, envName}:
let

  hpackFile = conf:
  builtins.toFile "package.yaml" (builtins.toJSON (removeAttrs conf ["passthru"]));

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
  ${config.build-tools.hpack.package}/bin/hpack --force
  rm -f package.yaml
  '';

  depPkg = spec: let
    name =
    if lib.isAttrs spec
    then spec.name
    else spec;
    in lib.head (lib.splitString ":" (lib.head (lib.splitString " " name)));

  drvWith = conf: { pkgs, self, hsLib, ... }: pname: pkg:
  let
    attr = n:
    if lib.hasAttr n conf
    then conf.${n}
    else throw "The Cabal config for '${pname}' is missing the mandatory attribute '${n}'.";

    dep = n:
    if lib.hasAttr n self
    then self.${n}
    else
    throw "The Cabal config for '${pname}' in the env '${envName}' has a dependency on the nonexistent package '${n}'.";

    depspec = spec: let
      name = depPkg spec;
    in
    if name == pname
    then []
    else [(dep name)];

    deps = c: lib.concatMap depspec (c.dependencies or []);

    compDeps = sort: lib.concatMap deps (lib.attrValues (conf.${sort} or {}));

  in self.callPackage ({mkDerivation}: mkDerivation ({
    inherit pname;
    src =
      if config.genCabalInDerivations
      then srcWithCabal pkgs conf pname pkg.src
      else pkg.src;
    version = attr "version";
    license = attr "license";
    libraryHaskellDepends = deps (conf.library or {}) ++ compDeps "internal-libraries";
    executableHaskellDepends = compDeps "executables";
    testHaskellDepends = compDeps "tests";
    benchmarkHaskellDepends = compDeps "benchmarks";
  } // conf.passthru or {})) {};

  drv = api: pname:
  drvWith config.hpack.internal.packages.${pname} api pname;

in {
  inherit drvWith drv;
}
