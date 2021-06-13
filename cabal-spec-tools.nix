{ pkgs, profiling, }:
let
  inherit (pkgs.lib.attrsets) isDerivation;

  hl = pkgs.haskell.lib;

  unbreak = hl.unmarkBroken;

  noProfiling = hl.disableLibraryProfiling;

  yesProfiling = hl.enableLibraryProfiling;

  globalProfiling = if profiling then yesProfiling else noProfiling;

  mkSpec = _spec_type: data: {
    options = {};
    inherit _spec_type;
  } // data;

  drv = d: mkSpec "derivation" { drv = d; };

  minimalDrv = p:
  hl.dontHaddock (hl.dontBenchmark (hl.dontCheck (unbreak p)));
in {
  inherit unbreak drv globalProfiling noProfiling minimalDrv mkSpec;

  wrapDrv = spec: if isDerivation spec then drv spec else spec;

  transforms = { transforms ? [], ... }: transforms;

  profiling = yesProfiling;

  minimalProf = p: globalProfiling (minimalDrv p);
}
