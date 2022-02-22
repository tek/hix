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
  else if failed then throw "invalid package path ${pp} for base ${base}" else new
  else pp;

  relativePackages = base: mapAttrs (_: packageSubpath base);

  normalizeOverrides = old: deps:
  let
    local = if isAttrs old then old else { all = old; dev = old; };
    norm = mapAttrs (_: o: if isList o then o else [o]) local;
    depOverrides = map (o: o.overrides) deps;
  in { all = []; } // zipAttrsWith (_: concatLists) (depOverrides ++ [norm]);

  overridesFor = o: n:
  let c = o.${n} or [];
  in if isList c then c else [c];

  packagePath = base: pp:
  if builtins.isPath pp
  then pp
  else "${base}/${pp}";

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

  foldMapAttrs = f: xs:
  foldl' (z: a: z // a) {} (map f xs);

in {
  inherit
  packageSubpath
  relativePackages
  normalizeOverrides
  overridesFor
  packagePath
  asFunction
  unlines
  withModules
  foldMapAttrs
  ;
}
