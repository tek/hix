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

  normalizeOverrides = old:
  if isAttrs old
  then old
  else { all = old; };

  override = o: n:
  let c = o.${n} or [];
  in if builtins.isList c then c else [c];

  overridesFor = o: n:
  override o "all" ++ override o n;

in {
  inherit packageSubpath relativePackages normalizeOverrides overridesFor;
}
