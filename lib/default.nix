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

  withModules = config: extra: f:
  let
    current = attrByPath ["internal" "parents"] [] config ++ extra;
    newModules = lib.evalModules { modules = current ++ [(parents current)]; };
  in f newModules.config;

  foldAttrs =
  foldl' (z: a: z // a) {};

  foldMapAttrs = f: xs:
  foldAttrs (map f xs);

  over = path: f: attrs:
  if hasAttrByPath path attrs
  then updateManyAttrsByPath [{ inherit path; update = f; }] attrs
  else attrs;

in {
  inherit
  packageSubpath
  relativePackages
  mergeOverrides
  normalizeOverrides
  overridesFor
  asFunction
  unlines
  withModules
  foldAttrs
  foldMapAttrs
  over
  ;

  overrides = import ./overrides.nix { inherit lib; };
}
