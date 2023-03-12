{ lib }:
with lib;
let

  deprecated = fatal: option: advice:
  (if fatal then throw else warn) "The option '${option}' is deprecated. ${advice}.";

  renameAdvice = new: "Please use '${new}'";

  deprecatedRename = fatal: old: new:
  deprecated fatal old (renameAdvice new);

  deprecatedPath = fatal: path: advice: args:
  if hasAttrByPath path args
  then deprecated fatal (concatStringsSep "." path) advice args
  else args;

  deprecatedPathRename = fatal: path: new: args:
  deprecatedPath fatal path (renameAdvice new) args;

  actIf = cond: path: f: args:
  if cond (attrByPath path null args) then f args else args;

  normalizeCompat = { compat ? true, ... }@args:
  let normalizedCompat = if isAttrs compat then compat else { enable = compat; };
  in args // { compat = normalizedCompat; };

  checkDeprecated = args:
  pipe args [
    (deprecatedPathRename true ["ghci" "extraArgs"] "ghci.args")
    (deprecatedPathRename true ["compiler"] "devGhc.compiler")
    (deprecatedPathRename true ["versionFile"] "hackage.versionFile")
    (deprecatedPath false ["ghci" "extensions"] "Extensions are now read from cabal files")
    (actIf isBool ["compat"] (deprecatedRenamePath false "compat" "compat.enable"))
    (actIf isFunction ["ghcid" "commands"] (throw "The option 'ghcid.commands' must be a module."))
    normalizeCompat
  ];

in {
  inherit deprecated deprecatedRename;
  check = checkDeprecated;
  warn = deprecatedRename false;
}
