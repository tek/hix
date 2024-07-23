{lib}:
with lib;
let

  flake-utils = import (builtins.fetchTarball {
    url = "https://github.com/numtide/flake-utils/archive/refs/tags/v1.0.0.tar.gz";
    sha256 = "0hynd4rbkbplxzl2a8wb3r8z0h17z2alhhdsam78g3vgzpzg0d43";
  });

  utilWithConfig = config:
  import ../lib/util.nix { inherit config lib; };

  utilModule = extra: {config, ...}: let
    util = utilWithConfig config // extra;
  in {
    _module.args = {
      inherit util;
      inherit (util) internal build outputs;
    };
  };

  packageSubpath = base: pp:
  let
    new = strings.removePrefix (toString base + "/") (toString pp);
    failed = new == toString pp;
  in
  if builtins.isPath pp || builtins.substring 0 1 (toString pp) == "/"
  then
  if pp == base then "."
  else if failed then throw "invalid package path ${pp} for base ${base}" else new
  else pp;

  relativePackages = base: mapAttrs (_: packageSubpath base);

  mergeOverrides = zipAttrsWith (_: concatLists);

  concatOverrides = foldl (a: b: toList a ++ toList b) [];

  normalizeOverrides = project: deps: depsFull:
  let
    local = if isAttrs project then project else { all = project; dev = project; };
    norm = mapAttrs (_: o: if isList o then o else [o]) local;
    depsOverrides = map (o: o.overrides // { local = []; }) deps;
    depsFullOverrides = map (o: o.overrides) depsFull;
  in { all = []; } // mergeOverrides (depsOverrides ++ depsFullOverrides ++ [norm]);

  concatMapAttrsToList = f: a: concatLists (mapAttrsToList f a);

  unwords = concatStringsSep " ";

  unlines = concatStringsSep "\n";

  unlinesMap = concatMapStringsSep "\n";

  unlinesConcatMap = f: xs: concatStringsSep "\n" (concatMap f xs);

  # catSets :: [Attrs a] -> Attrs a
  catSets = lib.mergeAttrsList;

  mapListCatAttrs = f: xs:
  catSets (map f xs);

  catValues = a:
  catSets (lib.attrValues a);

  foldMapAttrs = f: set:
  lib.foldlAttrs (z: name: attr: z // f name attr) {} set;

  over = path: f: attrs:
  if hasAttrByPath path attrs
  then updateManyAttrsByPath [{ inherit path; update = f; }] attrs
  else attrs;

  mapKeys = f: lib.mapAttrs' (k: v: lib.nameValuePair (f k v) v);

  mapValues = f: lib.mapAttrs (_: f);

  mergeAttr = name: a: b:
  if isDerivation a
  then
  if isDerivation b
  then throw ''
  'mergeAuto' can not be used with sets containing competing derivations!
    Key: ${name}
    Derivation names: ${a.pname} / ${b.pname}
  ''
  else b // a
  else if isDerivation b
  then a // b
  else if isAttrs a
  then mergeAttrset a b
  else if isList a
  then a ++ b
  else b;

  mergeAttrset = l: r:
  let
    f = name:
    if hasAttr name l && hasAttr name r
    then mergeAttr name l.${name} r.${name}
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
  foldl mergeAuto z items;

  # Later items have precedence
  mergeAll = items:
  if length items == 0
  then throw "Internal error: passed empty list to 'mergeAll'."
  else mergeAll' (head items) (tail items);

  mergeAllAttrs = items:
  if length items == 0
  then {}
  else mergeAll' (head items) (tail items);

  mergeValues = a:
  mergeAllAttrs (lib.attrValues a);

  # Attrs (Maybe a) -> Attrs a
  catMaybes = lib.filterAttrs (_: a: a != null);

  # (a -> Maybe b) -> Attrs a -> Attrs b
  mapMaybe = f: a: catMaybes (lib.mapAttrs f a);

  maybe = alt: f: a: if a == null then alt else f a;

  apMaybe = lib.mapNullable;

  filterNulls = filterAttrs (_: a: a != null);

  restrictKeys = allow: lib.filterAttrs (k: _: lib.elem k allow);

  removeKeys = lib.flip builtins.removeAttrs;

  toTitle = s:
  if stringLength s == 0
  then s
  else toUpper (substring 0 1 s) + substring 1 (-1) s;

  minGhc = version: env:
  versionAtLeast env.ghc.version version;

  empty = v: if isAttrs v then empty (attrNames v) else v == [];

  evalModules = modules: lib.evalModules { inherit modules; };

  evalConfig = modules: (evalModules modules).config;

  overridesVia = desc: o:
  if desc == null
  then o
  else if isAttrs o && o ? __source && o ? __functor
  then { __source = [desc] ++ o.__source; inherit (o) __functor; }
  else if isFunction o
  then { __source = [desc]; __functor = _: o; }
  else if isList o
  then map (overridesVia desc) o
  else if isAttrs o
  then mapAttrs (_: overridesVia) o
  else o
  ;

  cabalDepPackage = dep:
  if isAttrs dep
  then dep.name
  else dep;

  app = program: { type = "app"; program = "${program}"; };

  removeApp = a: removeAttrs a ["app" "type"];

  console = import ./console.nix { inherit lib; };

  loadConsole = let
    inherit (console) colors color bold chevrons chevronY chevronM chevronsH;
    colorFun = name: ''
    ${name}()
    {
      echo -e "${color colors.${name} "\${*}"}"
    }
    '';
  in ''
  message()
  {
    echo -e "${chevrons} $*"
  }
  message_part()
  {
    echo -n -e "${chevrons} $*"
  }
  error_message()
  {
    echo -e "${chevrons} ${color colors.red "\${*}"}"
  }
  ${unlines (map colorFun (attrNames colors))}
  bold()
  {
      echo -e "${bold "$*"}"
  }
  chevronsH='${chevronsH}'
  chevrons='${chevrons}'
  chevronY='${chevronY}'
  chevronM='${chevronM}'
  die()
  {
    error_message $*
    exit 1
  }
  ask() {
    setopt local_options no_err_exit no_err_return
    local decision=""
    message_part "$1 [Yn] "
    read -k decision
    if [[ $decision != '\n' ]]
    then
      echo ""
    fi
    [[ $decision != 'n' ]]
  }
  '';

in {
  inherit
  lib
  utilWithConfig
  utilModule
  flake-utils
  packageSubpath
  relativePackages
  mergeOverrides
  concatOverrides
  normalizeOverrides
  concatMapAttrsToList
  unwords
  unlines
  unlinesMap
  unlinesConcatMap
  catSets
  mapListCatAttrs
  catValues
  foldMapAttrs
  over
  mapKeys
  mapValues
  mergeAttr
  mergeAttrset
  mergeAuto
  mergeAll'
  mergeAll
  mergeAllAttrs
  mergeValues
  catMaybes
  mapMaybe
  maybe
  apMaybe
  filterNulls
  restrictKeys
  removeKeys
  toTitle
  versionAtLeast
  minGhc
  empty
  evalModules
  evalConfig
  overridesVia
  cabalDepPackage
  app
  removeApp
  console
  loadConsole
  ;

  version = import ./version.nix { inherit lib; };
}
