{
  pkgs,
  self,
  super,
}:
with builtins;
let
  inherit (pkgs.lib.attrsets) filterAttrs foldAttrs isDerivation mapAttrs' nameValuePair;
  inherit (pkgs.lib.lists) foldl;
  inherit (pkgs.lib.debug) traceVal;
  hl = pkgs.haskell.lib;

  unbreak = hl.unmarkBroken;
  notest = p:
    hl.doHaddock (hl.dontBenchmark (hl.dontCheck (unbreak p)));
  hackageDirect = { pkg, ver, sha256 }:
    notest (self.callHackageDirect { inherit pkg ver sha256; } {});
  cabal2nix = name: src:
    notest (self.callCabal2nix name src {});
  cabal2nixNoHpack = name: src:
    notest (self.callCabal2nixWithOptions name src "--no-hpack" {});
  subPkg = dir: name: src:
    notest (self.callCabal2nixWithOptions name src "--subpath ${dir}" {});
  subPkgNoHpack = dir: name: src:
    notest (self.callCabal2nixWithOptions name src "--subpath ${dir} --no-hpack" {});

  drv = d: { _spec_type = "derivation"; drv = d; };

  wrapDrv = spec: if isDerivation spec then drv spec else spec;

  transforms = { transforms ? [], ... }: transforms;

  transform = hook: spec:
  let spec' = wrapDrv spec;
  in spec' // { transforms = ([hook] ++ (transforms spec')); };

  transformers = {
    jailbreak = transform hl.doJailbreak;
    configure = flag: transform (drv: hl.appendConfigureFlag drv flag);
    override = conf: transform (drv: hl.overrideCabal drv conf);
  };

  hackage = ver: sha256: { _spec_type = "hackage"; inherit ver sha256; };

  source = rec {
    root = src: { _spec_type = "root"; inherit src; };
    sub = src: path: { _spec_type = "sub"; inherit src path; };
    package = src: path: sub src "packages/${path}";
  };

  conditional = condition: { _spec_type = "conditional"; inherit condition; };

  keep = { _spec_type = "keep"; };

  only = target: spec: conditional (comp: if comp == "ghc${target}" then spec else keep);

  versions = vs: conditional (comp: vs.${comp} or keep);

  condPackage = compiler: pkg: spec:
  if spec._spec_type == "conditional"
  then spec.condition compiler
  else spec;

  normalize = compiler: pkg: spec:
  let spec' = condPackage compiler pkg (wrapDrv spec);
  in
  if !(spec' ? _spec_type)
  then throw "spec for ${pkg} must have attr `_spec_type` or be a derivation: ${spec'}"
  else spec';

  specDerivation = pkg: spec:
  if spec._spec_type == "hackage" then hackageDirect { inherit pkg; inherit (spec) ver sha256; }
  else if spec._spec_type == "root" then cabal2nix pkg spec.src
  else if spec._spec_type == "sub" then subPkg spec.path pkg spec.src
  else if spec._spec_type == "derivation" then spec.drv
  else if spec._spec_type == "keep" then super.${pkg} or null
  else throw "invalid dependency spec _spec_type for ${pkg}: ${spec}";

  applyTransformers = src: trans: trans src;

  package = pkg: spec: foldl applyTransformers (specDerivation pkg spec) (transforms spec);

  packages = compiler: ps: mapAttrs package (mapAttrs (normalize compiler) ps);
in transformers // {
  inherit unbreak notest hackageDirect cabal2nix cabal2nixNoHpack subPkg subPkgNoHpack;
  inherit packages source hackage conditional only versions self super pkgs keep drv transform;
  hsLib = hl;
}
