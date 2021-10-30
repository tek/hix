{ pkgs }:
with pkgs.lib;
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
in {
  inherit packageSubpath relativePackages;
}
