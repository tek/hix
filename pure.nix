{
  overrides = f: { cabal, compiler, ... }: _: _: cabal.packages compiler (f cabal);

  noOverrides = _: _: _: {};

  composeCabal = builtins.foldl' (a: z: c: (z c) // a c) (_: {});
}
