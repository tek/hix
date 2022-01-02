{ pkgs, profiling, }:
with pkgs.lib;
let

  hl = pkgs.haskell.lib;

  unbreak = hl.unmarkBroken;

  noProfiling = hl.disableLibraryProfiling;

  yesProfiling = hl.enableLibraryProfiling;

  globalProfiling = if profiling then yesProfiling else noProfiling;

  hackageDrv = p:
  hl.dontBenchmark (hl.dontCheck (unbreak p));

  minimalDrv = p:
  hl.dontHaddock (hackageDrv p);
in {
  inherit unbreak globalProfiling noProfiling minimalDrv hackageDrv;

  profiling = yesProfiling;

  minimalProf = p: globalProfiling (minimalDrv p);

  fast = p: noProfiling (hl.dontHaddock p);
}
