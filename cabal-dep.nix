{
  pkgs,
  profiling ? true,
}:
with builtins;
let
  cabalSpec = import ./cabal-spec.nix { inherit pkgs profiling; };
  tools = import ./cabal-spec-tools.nix { inherit pkgs profiling; };

  inherit (pkgs) lib;
  inherit (lib) composeExtensions;
  inherit (lib.attrsets) filterAttrs foldAttrs isDerivation mapAttrs' nameValuePair;
  inherit (lib.lists) foldl;
  inherit (lib.debug) traceVal;
  hl = pkgs.haskell.lib;

  options = name: default: spec:
  (spec.options or {}).${name} or default;

  hackageDirect = self: { pkg, ver, sha256 }:
    tools.hackageDrv (self.callHackageDirect { inherit pkg ver sha256; } {});

  cabal2nix = self: name: opts: src:
  tools.globalProfiling (self.callCabal2nixWithOptions name src opts {});

  subPkg = self: dir: name: opts: src:
  tools.globalProfiling (self.callCabal2nixWithOptions name "${src}/${dir}" opts {});

  condPackage = name: version: pkg: spec:
  if spec._spec_type == "conditional"
  then spec.condition name version
  else spec;

  optsOrDef = spec: spec.options or {};

  normalize = name: version: super: pkg: rawSpec:
  let
    run = spec: options:
    let
      wrapped = tools.wrapDrv spec;
      newOptions = options // optsOrDef spec;
    in
    if isFunction wrapped then run (wrapped super.${pkg}) newOptions
    else if !(wrapped ? _spec_type) then throw "spec for ${pkg} must have attr `_spec_type` or be a derivation"
    else if wrapped._spec_type == "conditional" then run (wrapped.condition name version) newOptions
    else wrapped // { options = newOptions; };
  in run rawSpec {};

  specDerivation = self: super: pkg: spec:
  if spec._spec_type == "hackage" then hackageDirect self { inherit pkg; inherit (spec) ver sha256; }
  else if spec._spec_type == "root" then cabal2nix self pkg (options "cabal2nix" "" spec) spec.src
  else if spec._spec_type == "sub" then subPkg self spec.path pkg (options "cabal2nix" "" spec) spec.src
  else if spec._spec_type == "derivation" then spec.drv
  else if spec._spec_type == "output" then spec.input.packages.${pkgs.system}.${pkg}
  else if spec._spec_type == "keep" then super.${pkg} or null
  else throw "invalid dependency spec _spec_type for ${pkg}: ${spec}";

  applyTransformers = src: trans: trans src;

  package = self: super: pkg: spec:
  foldl applyTransformers (specDerivation self super pkg spec) (tools.transforms spec);

  packages = overlay: self: super:
  let
    name = self.ghc.name;
    version = self.ghc.version;
    norm = normalize name version super;
  in mapAttrs (package self super) (mapAttrs norm (overlay (cabalSpec { inherit self super; })));

  asList = overlays:
  if isList overlays then overlays else [overlays];

  composeManyExtensions =
    lib.foldr composeExtensions (self: super: {});

  compose = overlays: composeManyExtensions (map packages (asList overlays));

  override = ghc: f: ghc.override { overrides = compose f; };
in {
  inherit packages compose tools override;
}
