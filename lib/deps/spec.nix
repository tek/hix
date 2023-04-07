{ lib, }:
with lib;
rec {

  initial = makeExtensible (_: {
    drv = null;
    transform = id;
    options = {};
  });

  zero = { __spec = _: id; };

  call = spec:
  if isAttrs spec
  then
    if isDerivation spec
    then _: old: old // { drv = spec; }
    else spec.__spec
  else (spec zero).__spec;

  create = f: prev: {
    __spec = args: old: (call prev args old).extend (final: prev: f (args // {
      inherit final prev;
      hsLib = args.pkgs.haskell.lib;
      lib = pkgs.lib;
    }));
  };

  set = spec: create (_: spec);

  transform = f: create (args@{ prev, ... }: { transform = flip pipe [prev.transform (f args)]; });

  transform_ = f: transform (_: f);

  option = name: value: create ({ prev, ... }: {
    options = prev.options // {
      ${name} = prev.options.${name} or [] ++ [value];
    };
  });
}
