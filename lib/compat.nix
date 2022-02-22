{ lib }:
with lib;
let

  deprecated = fatal: old: new:
  (if fatal then throw else warn) "The option '${old}' is deprecated. Please use '${new}'.";

  deprecatedPath = fatal: path: new: args:
  if hasAttrByPath path args
  then deprecated fatal (concatStringsSep "." path) new args
  else args;

  actIf = cond: path: f: args:
  if cond (attrByPath path null args) then f args else args;

  normalizeCompat = { compat ? true, ... }@args:
  let normalizedCompat = if isAttrs compat then compat else { enable = compat; };
  in args // { compat = normalizedCompat; };

  checkDeprecated = args:
  pipe args [
    (deprecatedPath true ["ghci" "extraArgs"] "ghci.args")
    (deprecatedPath true ["compiler"] "devGhc.compiler")
    (deprecatedPath true ["versionFile"] "hackage.versionFile")
    (actIf isBool ["compat"] (deprecated false "compat" "compat.enable"))
    (actIf isFunction ["ghcid" "commands"] (throw "The option 'ghcid.commands' must be a module."))
    normalizeCompat
  ];

in {
  inherit deprecated;
  check = checkDeprecated;
  warn = deprecated false;
}
