{ lib }:
with lib;
let

  joinPath = concatStringsSep ".";

  deprecated = fatal: option: advice:
  (if fatal then throw else warn) "The option '${option}' is deprecated. ${advice}.";

  renameAdvice = new: "Please use '${new}'";

  deprecatedRename = fatal: old: new:
  deprecated fatal old (renameAdvice new);

  deprecatedPath = fatal: path: advice: args:
  if hasAttrByPath path args
  then deprecated fatal (joinPath path) advice args
  else args;

  deprecatedPathRename = fatal: path: new: args:
  deprecatedPath fatal path (renameAdvice new) args;

  notAttrs = a: !(isAttrs a);

  actIf = cond: path: f: args: let
    target = attrByPath path null args;
  in if target != null && cond target then f args else args;

  actIfAny = cond: path: f: args: let
    target = attrByPath path null args;
  in if target == null
  then args
  else if !(isAttrs target)
  then throw "The option '${joinPath path}' must be an attrset."
  else if any cond (attrValues target) then f args else args;

  normalizeCompat = args:
  if args ? compat && isBool args.compat
  then args // { compat.enable = args.compat; }
  else args;

  packagesError = ''
  The type of the option 'packages.<package-name>' has been changed from a path to a module.
  To get the old behaviour, use 'packages.<package-name>.src = ./foo'.
  '';

  preludeError = ''
  The Prelude is now read from cabal files for ghci and can be set in 'packages.cabal.prelude'
  '';

  checkDeprecated = args:
  pipe args [
    (deprecatedPathRename true ["compat" "projects"] "compat.versions")
    (deprecatedPathRename true ["ghci" "extraArgs"] "ghci.args")
    (deprecatedPathRename true ["compiler"] "devGhc.compiler")
    (deprecatedPathRename true ["versionFile"] "hackage.versionFile")
    (deprecatedPath true ["ghci" "extensions"]
      "Extensions are now read from cabal files for ghci and can be set in 'packages.cabal.extensions'")
    (deprecatedPath true ["ghci" "preludePackage"] preludeError)
    (deprecatedPath true ["ghci" "preludeModule"] preludeError)
    (actIf notAttrs ["compat"] (deprecatedRename false "compat" "compat.enable"))
    (actIf isFunction ["ghcid" "commands"] (throw "The option 'ghcid.commands' must be a module."))
    (actIfAny notAttrs ["packages"] (throw packagesError))
    (deprecatedPath true ["hpack" "packages"] "Cabal config has been moved to the top-level option 'packages'")
    normalizeCompat
  ];

  checkDeprecated' = mod:
  if isFunction mod
  then args: checkDeprecated (mod args)
  else checkDeprecated mod;

in {
  inherit deprecated deprecatedRename;
  check = checkDeprecated';
  warn = deprecatedRename false;
}
