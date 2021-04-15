{
  packages ? {},
}:
rec {
  wantReduce = { pname, ... }:
    pname != "ghc" && !(builtins.elem pname (builtins.attrNames packages));
  reduceDerivation = args: args // {
    doBenchmark = false;
    doCheck = false;
    doHoogle = false;
    doHaddock = false;
  };
  reduceIfWanted = profiling: args:
    if profiling && wantReduce args then reduceDerivation args else args;
  setProfiling = profiling: args:
    args // { enableLibraryProfiling = profiling; };
  derivationOverride = profiling: _: super: {
    mkDerivation = args:
    let
      finalArgs = setProfiling profiling (reduceIfWanted profiling args);
    in
      super.mkDerivation finalArgs;
  };
}
