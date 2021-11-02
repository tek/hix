{ lib }:
with lib;
let
  packageSubpath = base: pp:
  let
    new = strings.removePrefix (toString base + "/") (toString pp);
    failed = new == (toString pp);
  in
  if builtins.isPath pp
  then
  if pp == base then "."
  else if failed then builtins.abort "invalid package path ${pp} for base ${base}" else new
  else pp;

  relativePackages = base: mapAttrs (_: packageSubpath base);

  normalizeOverrides = old: deps:
  let
    local = if isAttrs old then old else { all = old; };
    norm = mapAttrs (_: o: if isList o then o else [o]) local;
    depOverrides = map (o: o.overrides) deps;
  in
    zipAttrsWith (_: concatLists) ([norm] ++ depOverrides);

  overridesFor = o: n:
  let c = o.${n} or [];
  in if isList c then c else [c];

in {
  inherit packageSubpath relativePackages normalizeOverrides overridesFor;
}
