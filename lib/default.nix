{ lib }:
with lib;
let

  packageSubpath = base: pp:
  let
    new = strings.removePrefix (toString base + "/") (toString pp);
    failed = new == toString pp;
  in
  if builtins.isPath pp
  then
  if pp == base then "."
  else if failed then throw "invalid package path ${pp} for base ${base}" else new
  else pp;

  relativePackages = base: mapAttrs (_: packageSubpath base);

  mergeOverrides = zipAttrsWith (_: concatLists);

  concatOverrides = foldl' (a: b: toList a ++ toList b) [];

  normalizeOverrides = project: deps: depsFull:
  let
    local = if isAttrs project then project else { all = project; dev = project; };
    norm = mapAttrs (_: o: if isList o then o else [o]) local;
    depsOverrides = map (o: o.overrides // { local = []; }) deps;
    depsFullOverrides = map (o: o.overrides) depsFull;
  in { all = []; } // mergeOverrides (depsOverrides ++ depsFullOverrides ++ [norm]);

  overridesFor = o: n:
  let c = o.${n} or [];
  in if isList c then c else [c];

  asFunction = f:
  if isFunction f then f else _: f;

  unlines = concatStringsSep "\n";

  parents = modules: {
    options.internal.parents = mkOption { type = types.unspecified; };
    config.internal.parents = modules;
  };

  modulesRaw = config: extra:
  let
    current = attrByPath ["internal" "parents"] [] config ++ extra;
    newModules = lib.evalModules { modules = current ++ [(parents current)]; };
  in newModules;

  withModules = config: extra: f:
  f (modulesRaw config extra).config;

  foldAttrs =
  foldl' lib.mergeAttrs {};

  foldMapAttrs = f: xs:
  foldAttrs (map f xs);

  over = path: f: attrs:
  if hasAttrByPath path attrs
  then updateManyAttrsByPath [{ inherit path; update = f; }] attrs
  else attrs;

  mergeAttr = a: b:
  if isAttrs a
  then mergeAttrset a b
  else if isList a
  then a ++ b
  else b;

  mergeAttrset = l: r:
  let
    f = name:
    if hasAttr name l && hasAttr name r
    then mergeAttr l.${name} r.${name}
    else l.${name} or r.${name};
  in genAttrs (concatMap attrNames [l r]) f;

  mergeAuto = a: b:
  if b == null
  then a
  else if isList a
  then a ++ b
  else if isAttrs a
  then mergeAttrset a b
  else b;

  mergeAll' = z: items:
  foldl' mergeAuto z items;

  mergeAll = items:
  if length items == 0
  then throw "Internal error: passed empty list to 'mergeAll'."
  else mergeAll' (head items) (tail items);

  toTitle = s:
  if stringLength s == 0
  then s
  else toUpper (substring 0 1 s) + substring 1 (-1) s;

  minGhc = version: env:
  versionAtLeast env.ghc.version version;

  empty = v: if isAttrs v then empty (attrNames v) else v == [];

in {
  inherit
  packageSubpath
  relativePackages
  mergeOverrides
  concatOverrides
  normalizeOverrides
  overridesFor
  asFunction
  unlines
  modulesRaw
  withModules
  foldAttrs
  foldMapAttrs
  over
  mergeAttr
  mergeAttrset
  mergeAuto
  mergeAll'
  mergeAll
  toTitle
  minGhc
  empty
  ;

  overrides = import ./overrides.nix { inherit lib; };
  ghcOverlay = import ./ghc-overlay.nix;
}
