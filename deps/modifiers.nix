{ pkgs, }:
with pkgs.lib;
let

  hl = pkgs.haskell.lib;

  unbreak = hl.unmarkBroken;

  noProfiling = hl.disableLibraryProfiling;

  yesProfiling = hl.enableLibraryProfiling;

  notest = hl.dontCheck;

  bench = hl.doBenchmark;

  nobench = hl.dontBenchmark;

  hackageDrv = p:
  hl.dontBenchmark (notest (unbreak p));

  minimalDrv = p:
  hl.dontHaddock (hackageDrv p);
in {
  inherit unbreak noProfiling minimalDrv hackageDrv notest bench nobench;

  profiling = yesProfiling;

  fast = p: noProfiling (hl.dontHaddock p);
}
