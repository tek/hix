{lib}: let

  normalize = spec:
  if lib.isAttrs spec
  then { mixin = []; } // spec
  else let
    result = builtins.split "^([[:graph:]]+) (.*)$" spec;
    parts = lib.head (lib.drop 1 result);
    name = lib.head parts;
    version = lib.head (lib.drop 1 parts);
  in
  if lib.length result >= 3
  then { inherit name version; mixin = []; }
  else { name = spec; version = null; mixin = []; };

  mainLibName = full: let
    result = builtins.split "^(.*):.*$" full;
    parts = lib.head (lib.drop 1 result);
    name = lib.head parts;
  in
  if lib.length result >= 3
  then name
  else full;

  trivial = v: v == null || v == "0" || v == ">=0" || v == "";

  nontrivial = v: !(trivial v);

  intersectAtomic = f1: f2: v1: v2:
  if trivial v1
  then
    if trivial v2
    then null
    else f2 v2
  else
    if trivial v2
    then f1 v1
    else "(${f1 v1}) && (${f2 v2})";

  intersect = intersectAtomic lib.id lib.id;

  intersectManaged =
  intersectAtomic (v: ">=${v}") (v: "<${v}");

  normalizeManaged = value:
  if value == null
  then null
  else if lib.isString value
  then value
  else if lib.isAttrs value
  then intersectManaged (value.lower or null) (value.upper or null)
  else throw "Invalid managed bounds in state file: ${builtins.toString value}";

in {
  inherit normalize mainLibName trivial nontrivial intersect normalizeManaged;
}
