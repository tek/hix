{ lib, }:
with lib;
let

  isOC = oc: isAttrs oc && oc ? __hix_oc;

  listOC = oc:
  if isDerivation oc
  then [(drv oc).single]
  else if isList oc
  then oc
  else if oc ? multi
  then oc.multi
  else if oc ? single
  then [oc.single]
  else throw "Bad value for listOC: ${generators.toPretty {} oc}";

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
    then { found = false; result = result ++ [spec]; }
    else { found = false; inherit result; };
  in (foldl folder { found = false; result = []; } specs).result;

# ----------------------------------------------------------------------------------------------------------------------

  compile = specs: let

    check = acc: spec: specs:
    if spec.type == "decl"
    then acc // { decl = spec; }
    else if spec.type == "transform"
    then spin (acc // { trans = acc.trans ++ [spec]; }) specs
    else if spec.type == "option"
    then spin (acc // { options = acc.options // { ${spec.name} = acc.options.${spec.name} or [] ++ [spec.meta]; }; }) specs
    else if spec.type == "no-pregen"
    then spin acc
    else throw "invalid spec type '${spec.type}'";

    spin = acc: specs:
      if specs == []
      then acc
      else check acc (head specs) (tail specs);

  in spin { decl = null; trans = []; options = {}; } (listOC specs);

# ----------------------------------------------------------------------------------------------------------------------

  reifyComp = args: comp: let

    inherit (args) pkg;
    inherit (comp) decl;

    drv =
      if decl == null
      then args.super.${pkg} or null
      else decl.impl args comp.options;

    noDecl = throw ''
    The override for '${pkg}' does not declare a derivation and the default package set does not contain '${pkg}'!
    '';

    apply = drv: trans:
    trans.impl drv args comp.options;

    transformed = foldl apply drv comp.trans;

    final =
      if comp.trans == []
      then drv
      else
      if drv == null
      then noDecl
      else transformed;

  in final;

  reify = args: specs: reifyComp args (compile specs);

# ----------------------------------------------------------------------------------------------------------------------

  call = name: f: args: options: f (args // { inherit options; });

  finalDecl = specs:
  findFirst (a: a.type == "decl") null specs;

  renderPregen = self: ''
  {
    meta = ${generators.toPretty { indent = "  "; } self.meta};
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

  reifyPregen = args: specs: let
    norm = listOC specs;
    spec = finalDecl norm;
    options = collectOptions norm;
    pregen = if spec == null then null else runPregen args options spec;
  in if pregen == null then noPregen else pregen;

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

  pregen = src: impl: old: old // { single = old.single // {  pregen = { enable = true; inherit src impl; }; }; };

  drv = d: decl "drv" "Explicit derivation" { drv = d; } (meta: _: meta.drv);

in {
  inherit transform decl option pregen drv isOC listOC show compile reifyComp reify reifyPregen;
  transform_ = name: f: transform name name {} (_: _: f);
}
