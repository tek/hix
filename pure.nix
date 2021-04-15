{
  overrides = f: { cabal, compiler, ... }: _: _: cabal.packages compiler (f cabal);

  noOverrides = _: _: _: {};

  composeCabal = builtins.foldl' (a: z: c: (z c) // a c) (_: {});

  packagePath = base: pp:
  if builtins.isPath pp
  then pp
  else "${base}/${pp}";
}
