{ config, lib, ... }:
with builtins;
with lib;
let

  hpackFile = name:
  toFile "package.yaml" (toJSON (removeAttrs config.hpack.packages.${name} ["passthru"])); 

  withCabal = name: src: config.pkgs.runCommand "${name}-gen-cabal" {} ''
  cp -r ${src} $out
  chmod u+w $out
  cd $out
  cp ${hpackFile name} package.yaml
  ${config.internal.basicGhc.hpack}/bin/hpack
  rm package.yaml
  '';

  depPkg = n:
  if isAttrs n
  then n.name
  else head (splitString " " n);

  simpleCabalDrv = { pkgs, self, hsLib, ... }: pname: src:
  let

    conf = config.hpack.packages.${pname};

    attr = n:
    if hasAttr n conf
    then conf.${n}
    else throw "The Cabal config for '${pname}' is missing the mandatory attribute '${n}'.";

    dep = n:
    if hasAttr n self
    then self.${n}
    else throw "The Cabal config for '${pname}' has a dependency on the nonexistent package '${n}'.";

    depspec = n:
    if n == pname
    then []
    else [(dep (depPkg n))];

    deps = c: concatMap depspec (c.dependencies or []);

  in self.mkDerivation ({
    inherit pname src;
    version = attr "version";
    license = attr "license";
    libraryHaskellDepends = deps (conf.library or {});
    executableHaskellDepends = concatMap deps (attrValues (conf.executables or {}));
    testHaskellDepends = concatMap deps (attrValues (conf.tests or {}));
  } // conf.passthru or {});

  inferCabalDrv = api: name: src: throw "No Cabal config for '${name}' and 'ifd = true'.";

in {
  inherit withCabal simpleCabalDrv inferCabalDrv;
}
