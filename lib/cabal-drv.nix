{ config, lib, util, ... }:
with builtins;
with lib;
let

  hpackFile = conf:
  toFile "package.yaml" (toJSON (removeAttrs conf ["passthru"]));

  srcWithCabal = pkgs: conf: name: src: let
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

  drvWith = preConf: { pkgs, self, hsLib, ... }: pname: pkg:
  let

    processCondition = dep: let
      inherit (dep) condition;
    in
      if condition == null || condition.type == "verbatim"
      then [dep]
      else if lib.hasAttr condition.type config.conditions
      then
      if (config.conditions.${condition.type} condition.args).satisfied self
      then [dep]
      else []
      else throw "Invalid condition type '${condition.type}': Not defined in 'config.conditions'.";

    conf = util.mapComponents (util.overDependencies (concatMap processCondition)) preConf;

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

    multiDeps = sort: concatMap deps (attrValues (conf.${sort} or {}));

  # TODO setupDepends?
  in self.callPackage ({mkDerivation}: mkDerivation ({
    inherit pname;
    src = srcWithCabal pkgs conf pname pkg.src;
    version = attr "version";
    license = attr "license";
    libraryHaskellDepends = deps (conf.library or {}) ++ multiDeps "internal-libraries";
    executableHaskellDepends = multiDeps "executables";
    testHaskellDepends = multiDeps "tests";
    benchmarkHaskellDepends = multiDeps "benchmarks";
  } // conf.passthru or {})) {};

  drv = api: pname:
  drvWith config.hpack.internal.packages.${pname} api pname;

in {
  inherit drv;
}
