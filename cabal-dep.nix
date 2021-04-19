{
  pkgs,
  compiler,
}:
with builtins;
let
  cabalSpec = import ./cabal-spec.nix;
  tools = import ./cabal-spec-tools.nix { inherit pkgs; };

  inherit (pkgs.lib) composeManyExtensions;

  inherit (pkgs.lib.attrsets) filterAttrs foldAttrs isDerivation mapAttrs' nameValuePair;
  inherit (pkgs.lib.lists) foldl;
  inherit (pkgs.lib.debug) traceVal;
  hl = pkgs.haskell.lib;

  hackageDirect = self: { pkg, ver, sha256 }:
    tools.minimalDrv (self.callHackageDirect { inherit pkg ver sha256; } {});
  cabal2nix = self: name: src:
    self.callCabal2nix name src {};
  subPkg = self: dir: name: src:
    self.callCabal2nix name "${src}/${dir}" {};

  condPackage = pkg: spec:
  if spec._spec_type == "conditional"
  then spec.condition compiler
  else spec;

  normalize = pkg: spec:
  let
    applied = if isFunction spec then spec super.${pkg} else spec;
    unwrappedCond = condPackage pkg (tools.wrapDrv applied);
  in
  if !(unwrappedCond ? _spec_type)
  then throw "spec for ${pkg} must have attr `_spec_type` or be a derivation: ${unwrappedCond}"
  else unwrappedCond;

  specDerivation = self: super: pkg: spec:
  if spec._spec_type == "hackage" then hackageDirect self { inherit pkg; inherit (spec) ver sha256; }
  else if spec._spec_type == "root" then cabal2nix self pkg spec.src
  else if spec._spec_type == "sub" then subPkg self spec.path pkg spec.src
  else if spec._spec_type == "derivation" then spec.drv
  else if spec._spec_type == "output" then spec.input.packages.${pkgs.system}.${pkg}
  else if spec._spec_type == "keep" then super.${pkg} or null
  else throw "invalid dependency spec _spec_type for ${pkg}: ${spec}";

  applyTransformers = src: trans: trans src;

  package = self: super: pkg: spec:
  foldl applyTransformers (specDerivation self super pkg spec) (tools.transforms spec);

  packages = ps: self: super:
  mapAttrs (package self super) (mapAttrs (normalize) (ps (cabalSpec { inherit pkgs self super; })));

  compose = os: composeManyExtensions (map packages os);
in {
  inherit packages compose;
}
