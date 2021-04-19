{
  base,
  pkgs,
  compiler,
  overrides ? _: {},
  packages ? {},
  cabal2nixOptions ? "",
  profiling ? false,
}:
let
  cabalDep = import ./cabal-dep.nix { inherit pkgs compiler; };
  pure = import ./pure.nix;
  tools = import ./tools.nix { inherit pkgs; };
  inherit (pkgs.haskell.lib) dontCheck dontHaddock dontBenchmark disableLibraryProfiling;

  reduceWork = d: disableLibraryProfiling (dontHaddock (dontBenchmark d));

  local = ghc: n: p: reduceWork (ghc.callCabal2nixWithOptions n (pure.packagePath base p) cabal2nixOptions {});

  projectPackages = self: _: builtins.mapAttrs (local self) packages;

  buildOverrides = cabalDep.compose overrides;
in
  pkgs.lib.composeExtensions projectPackages buildOverrides
