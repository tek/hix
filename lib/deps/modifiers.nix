{ pkgs, }:
let

  hl = pkgs.haskell.lib;

  unbreak = hl.unmarkBroken;

  jailbreak = hl.doJailbreak;

  noprofiling = hl.disableLibraryProfiling;

  profiling = hl.enableExecutableProfiling;

  notest = hl.dontCheck;

  bench = hl.doBenchmark;

  nobench = hl.dontBenchmark;

  nodoc = hl.dontHaddock;

  noshared = hl.disableSharedLibraries;

  shared = hl.enableSharedExecutables;

  minimal = p: noprofiling (nodoc (nobench (notest (unbreak p))));

  fast = p: noprofiling (nodoc p);

  force = p: nobench (notest (unbreak (jailbreak p)));

  force' = p: nodoc (force p);

in {
  inherit
  unbreak
  jailbreak
  profiling
  noprofiling
  notest
  bench
  nobench
  nodoc
  noshared
  shared
  minimal
  fast
  force
  force';
}
