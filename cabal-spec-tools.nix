{ pkgs, profiling, }:
let
  inherit (pkgs.lib.attrsets) isDerivation;

  hl = pkgs.haskell.lib;

  unbreak = hl.unmarkBroken;

  noProfiling = hl.disableLibraryProfiling;

  yesProfiling = hl.enableLibraryProfiling;

  globalProfiling = if profiling then yesProfiling else noProfiling;

  drv = d: { _spec_type = "derivation"; drv = d; };

  minimalDrv = p:
  hl.dontHaddock (hl.dontBenchmark (hl.dontCheck (unbreak p)));
in {
  inherit unbreak drv globalProfiling noProfiling minimalDrv;

  wrapDrv = spec: if isDerivation spec then drv spec else spec;

  transforms = { transforms ? [], ... }: transforms;

  profiling = yesProfiling;

  minimalProf = p: globalProfiling (minimalDrv p);
}
