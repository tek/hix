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
  minimalDrv = p: hl.dontHaddock (hl.dontBenchmark (hl.dontCheck (unbreak p)));
  hackageDirect = { pkg, ver, sha256 }:
    minimalDrv (self.callHackageDirect { inherit pkg ver sha256; } {});
  cabal2nix = name: src:
    self.callCabal2nix name src {};
  subPkg = dir: name: src:
    self.callCabal2nix name "${src}/${dir}" {};

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
    minimal = transform minimalDrv;
  };

  hackage = ver: sha256: { _spec_type = "hackage"; inherit ver sha256; };

  source = rec {
    root = src: { _spec_type = "root"; inherit src; };
    sub = src: path: { _spec_type = "sub"; inherit src path; };
    package = src: path: sub src "packages/${path}";
    output = input: { _spec_type = "output"; inherit input; };
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
  else if spec._spec_type == "output" then spec.input.packages.${pkgs.system}.${pkg}
  else if spec._spec_type == "keep" then super.${pkg} or null
  else throw "invalid dependency spec _spec_type for ${pkg}: ${spec}";

  applyTransformers = src: trans: trans src;

  package = pkg: spec: foldl applyTransformers (specDerivation pkg spec) (transforms spec);

  packages = compiler: ps: mapAttrs package (mapAttrs (normalize compiler) ps);
in transformers // {
  inherit unbreak minimal hackageDirect cabal2nix subPkg;
  inherit packages source hackage conditional only versions self super pkgs keep drv transform;
  hsLib = hl;
  inherit (pkgs) system;
}
