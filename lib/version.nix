{lib}: let

  normalize = spec:
  if lib.isAttrs spec
  then spec
  else let
    result = builtins.split "^([[:graph:]]+) (.*)$" spec;
    parts = lib.head (lib.drop 1 result);
    name = lib.head parts;
    version = lib.head (lib.drop 1 parts);
  in
  if lib.length result >= 3
  then { inherit name version; }
  else { name = spec; version = null; };

  mainLibName = full: let
    result = builtins.split "^(.*):.*$" full;
    parts = lib.head (lib.drop 1 result);
    name = lib.head parts;
  in
  if lib.length result >= 3
  then name
  else full;

in {
  inherit normalize mainLibName;
}
