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

  hackageDrv = p:
  hl.dontBenchmark (hl.dontCheck (unbreak p));

  minimalDrv = p:
  hl.dontHaddock (hackageDrv p);
in {
  inherit unbreak drv globalProfiling noProfiling minimalDrv mkSpec hackageDrv;

  wrapDrv = spec: if isDerivation spec then drv spec else spec;

  transforms = { transforms ? [], ... }: transforms;

  profiling = yesProfiling;

  minimalProf = p: globalProfiling (minimalDrv p);
}
