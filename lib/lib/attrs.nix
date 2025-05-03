{lib}: let

  # catSets :: [Attrs a] -> Attrs a
  catSets = lib.mergeAttrsList;

  mapListCatAttrs = f: xs:
  catSets (map f xs);

  catValues = a:
  catSets (lib.attrValues a);

  foldMapAttrs = f: set:
  lib.foldlAttrs (z: name: attr: z // f name attr) {} set;

  over = path: f: attrs:
  if lib.hasAttrByPath path attrs
  then lib.updateManyAttrsByPath [{ inherit path; update = f; }] attrs
  else attrs;

  mapKeys = f: lib.mapAttrs' (k: v: lib.nameValuePair (f k v) v);

  mapValues = f: lib.mapAttrs (_: f);

  mergeAttr = name: a: b:
  if lib.isDerivation a
  then
  if lib.isDerivation b
  then throw ''
  'mergeAuto' can not be used with sets containing competing derivations!
    Key: ${name}
    Derivation names: ${a.pname or a.name or "<none>"} / ${b.pname or b.name or "<none>"}
  ''
  else b // a
  else if lib.isDerivation b
  then a // b
  else if lib.isAttrs a
  then mergeAttrset a b
  else if lib.isList a
  then a ++ b
  else b;

  mergeAttrset = l: r:
  let
    f = name:
    if lib.hasAttr name l && lib.hasAttr name r
    then mergeAttr name l.${name} r.${name}
    else l.${name} or r.${name};
  in lib.genAttrs (lib.concatMap lib.attrNames [l r]) f;

  mergeAuto = a: b:
  if b == null
  then a
  else if lib.isList a
  then a ++ b
  else if lib.isAttrs a
  then mergeAttrset a b
  else b;

  mergeAll' = z: items:
  lib.foldl mergeAuto z items;

  # Later items have precedence
  mergeAll = items:
  if lib.length items == 0
  then throw "Internal error: passed empty list to 'mergeAll'."
  else mergeAll' (lib.head items) (lib.tail items);

  mergeAllAttrs = items:
  if lib.length items == 0
  then {}
  else mergeAll' (lib.head items) (lib.tail items);

  mergeValues = a:
  mergeAllAttrs (lib.attrValues a);

  concatMapAttrsToList = f: a: lib.concatLists (lib.mapAttrsToList f a);

  empty = v: if lib.isAttrs v then empty (lib.attrNames v) else v == [];

in {

  inherit
  catSets
  mapListCatAttrs
  catValues
  foldMapAttrs
  over
  mapKeys
  mapValues
  mergeAttr
  mergeAttrset
  mergeAuto
  mergeAll'
  mergeAll
  mergeAllAttrs
  mergeValues
  concatMapAttrsToList
  empty
  ;

}
