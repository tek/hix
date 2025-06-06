{ lib, }:
let

  isOC = oc: lib.isAttrs oc && oc ? __hix_oc;

  listOC = oc:
  if lib.isDerivation oc
  then [(drv oc).single]
  else if lib.isList oc
  then oc
  else if oc ? multi
  then oc.multi
  else if oc ? single
  then [oc.single]
  else if oc == null
  then [disable.single]
  else throw "Bad value for listOC: ${lib.generators.toPretty {} oc}";

  composeOC = cur: prev: {
    multi = listOC cur ++ listOC prev;
    __hix_oc = true;
  };

  defaults = {
    pregen = {
      enable = false;
      src = null;
      impl = _: _: "no pregen";
    };
  };

  mkOC = conf: {
    single = defaults // conf;
    __functor = composeOC;
    __hix_oc = true;
  };

  showSpec = spec: { inherit (spec) name desc meta; };

  show = specs: builtins.toJSON (if specs ? multi then map showSpec specs.multi else [(showSpec specs.single)]);

  collectOptions = specs: let
    folder = z@{found, result}: spec:
    if found
    then z
    else if spec.type == "decl"
    then { found = true; inherit result; }
    else if spec.type == "option"
    then { found = false; result = result // { ${spec.name} = (result.${spec.name} or []) ++ [spec.meta]; }; }
    else { found = false; inherit result; };
  in (lib.foldl folder { found = false; result = {}; } specs).result;

# ----------------------------------------------------------------------------------------------------------------------

  compile = specs: let

    check = acc: spec: specs:
    if !(spec ? type)
    then throw "invalid override combinator without type attribute: ${lib.generators.toPretty {} spec}"
    else if spec.type == "decl"
    then acc // { decl = spec; }
    else if spec.type == "transform"
    then spin (acc // { trans = acc.trans ++ [spec]; }) specs
    else if spec.type == "option"
    then spin (acc // { options = acc.options // { ${spec.name} = (acc.options.${spec.name} or []) ++ [spec.meta]; }; }) specs
    else if spec.type == "no-pregen"
    then spin acc
    else throw "invalid spec type '${spec.type}'";

    spin = acc: specs:
      if specs == []
      then acc
      else check acc (lib.head specs) (lib.tail specs);

  in spin { decl = null; trans = []; options = {}; } (listOC specs);

# ----------------------------------------------------------------------------------------------------------------------

  # Construct a final derivation from a list of OCs.
  # Usually, these consist of zero or more derivation OCs and zero or more transformations.
  # If more than one derivation is present, `compile` will choose the last occurrence.
  # If no derivation is present, the package defined in the GHC set is obtained from `args.super` (which is provided by
  # the GHC set's `override` function).
  # If the GHC set does not define this package either, this override will be ignored – it likely means that the project
  # defines a global transformation override for a local package that isn't a target of the currently processed env.
  reifyComp = args: comp: let

    inherit (args) pkg;
    inherit (comp) decl;

    drv =
      if decl == null
      then args.super.${pkg} or null
      else decl.impl args comp.options;

    apply = drv: trans:
    trans.impl drv args comp.options;

    transformed = lib.foldl apply drv comp.trans;

    final =
      if comp.trans == [] || drv == null
      then drv
      else transformed;

  in final;

  reify = args: specs: reifyComp args (compile specs);

# ----------------------------------------------------------------------------------------------------------------------

  call = name: f: args: options: f (args // { inherit options; });

  finalDecl = specs:
  lib.findFirst (a: a.type == "decl") null specs;

  renderPregen = self: ''
  {
    meta = ${lib.generators.toPretty { indent = "  "; } self.meta};
    drv = ${self.drv};
  }
  '';

  runPregen = args: options: spec: let
    data = {
      __toString = renderPregen;
      inherit (spec) meta;
      drv = spec.pregen.impl spec.meta (args // { inherit options; });
    };
  in if spec.pregen.enable then data else null;

  noPregen = mkOC {
    name = "no-pregen";
    desc = "Placeholder for recursively used decls";
    meta = {};
    type = "no-pregen";
    impl = {};
  };

  # Entry point for applying OCs to a GHC set to pregenerate derivations in order to persist them to disk.
  reifyPregen = args: specs: let
    norm = listOC specs;
    spec = finalDecl norm;
    options = collectOptions norm;
    pregen = if spec == null then null else runPregen args options spec;
  in if pregen == null then noPregen else pregen;

# ----------------------------------------------------------------------------------------------------------------------

  transform = name: desc: meta: f: mkOC {
    inherit name desc meta;
    type = "transform";
    impl = drv: call name (args: f meta args drv);
  };

  decl = name: desc: meta: f: mkOC {
    inherit name desc meta;
    type = "decl";
    impl = call name (f meta);
  };

  option = name: desc: meta: mkOC {
    inherit name desc meta;
    type = "option";
    impl = meta;
  };

  pregen = src: impl: hydrate: old:
  old // {
    single = old.single // {
      pregen = {
        enable = true;
        inherit src impl hydrate;
      };
    };
  };

  drv = d: decl "drv" "Explicit derivation" { drv = d; } (meta: _: meta.drv);

  disable = decl "disable" "No derivation" { drv = null; } (meta: _: meta.drv);

in {
  inherit transform decl option pregen drv disable isOC listOC show compile reifyComp reify reifyPregen;
  transform_ = name: f: transform name name {} (_: _: f);
}
