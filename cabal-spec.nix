{
  pkgs,
  profiling,
}:
{
  self,
  super,
}:
with builtins;
let
  tools = import ./cabal-spec-tools.nix { inherit pkgs profiling; };

  inherit (pkgs.lib.attrsets) filterAttrs foldAttrs isDerivation mapAttrs' nameValuePair;
  inherit (pkgs.lib.lists) foldl;
  inherit (pkgs.lib.debug) traceVal;
  hl = pkgs.haskell.lib;

  transform = hook: spec:
  let unwrappedCond = tools.wrapDrv spec;
  in unwrappedCond // { transforms = ([hook] ++ (tools.transforms unwrappedCond)); };

  transformers = {
    jailbreak = transform hl.doJailbreak;
    configure = flag: transform (drv: hl.appendConfigureFlag drv flag);
    override = conf: transform (drv: hl.overrideCabal drv conf);
    minimal = transform tools.minimalDrv;
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

  only = target: spec: conditional (_: version: if version == target then spec else keep);

  versions = vs: conditional (_: version: vs.${version} or keep);
in transformers // {
  inherit (tools) unbreak minimalDrv minimalProf drv;
  inherit source hackage conditional only versions self super pkgs keep transform;
  hsLib = hl;
  inherit (pkgs) system;
}
