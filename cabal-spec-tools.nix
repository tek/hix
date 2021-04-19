{ pkgs }:
let
  inherit (pkgs.lib.attrsets) isDerivation;

  hl = pkgs.haskell.lib;

  unbreak = hl.unmarkBroken;

  drv = d: { _spec_type = "derivation"; drv = d; };
in {
  inherit unbreak drv;

  wrapDrv = spec: if isDerivation spec then drv spec else spec;

  transforms = { transforms ? [], ... }: transforms;

  minimalDrv = p: hl.dontHaddock (hl.dontBenchmark (hl.dontCheck (unbreak p)));
}
