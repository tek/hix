{ pkgs, }:
with pkgs.lib;
let

  hl = pkgs.haskell.lib;

  unbreak = hl.unmarkBroken;

  jailbreak = hl.doJailbreak;

  noprofiling = hl.disableLibraryProfiling;

  profiling = hl.enableLibraryProfiling;

  notest = hl.dontCheck;

  bench = hl.doBenchmark;

  nobench = hl.dontBenchmark;

  nodoc = hl.dontHaddock;

  minimal = p: noprofiling (nodoc (nobench (notest (unbreak p))));

in {
  inherit unbreak jailbreak profiling noprofiling minimal notest bench nobench nodoc;

  fast = p: noprofiling (nodoc p);
}
