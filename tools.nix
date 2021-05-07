{ pkgs }:
with pkgs.lib;
let
  packageSubpath = base: pp:
  let
    new = strings.removePrefix (toString base + "/") (toString pp);
    success = new == (toString pp);
  in
  if builtins.isPath pp
  then if success then builtins.abort "invalid package path ${pp}" else new
  else pp;

  relativePackages = base: mapAttrs (_: packageSubpath base);
in {
  inherit packageSubpath relativePackages;
}
