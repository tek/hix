{lib}:
with lib;
let

  flake-utils = import (builtins.fetchTarball {
    url = "https://github.com/numtide/flake-utils/archive/refs/tags/v1.0.0.tar.gz";
    sha256 = "0hynd4rbkbplxzl2a8wb3r8z0h17z2alhhdsam78g3vgzpzg0d43";
  });

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

  concatOverrides = foldl (a: b: toList a ++ toList b) [];

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

  concatMapAttrsToList = f: a: concatLists (mapAttrsToList f a);

  unwords = concatStringsSep " ";

  unlines = concatStringsSep "\n";

  unlinesMap = concatMapStringsSep "\n";

  unlinesConcatMap = f: xs: concatStringsSep "\n" (concatMap f xs);

  foldAttrs =
  foldl lib.mergeAttrs {};

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
  foldl mergeAuto z items;

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

  evalModules = modules: (lib.evalModules { inherit modules; }).config;

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

  loadConsole = let
    console = import ./console.nix { inherit lib; };
    inherit (console) colors color bold chevrons chevronY chevronM;
    colorFun = name: ''
    ${name}()
    {
      echo -e "${color colors.${name} "\${1}"}"
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
      echo -e "${bold "$1"}"
  }
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
    echo ""
    [[ $decision != 'n' ]]
  }
  '';

in {
  inherit
  lib
  flake-utils
  packageSubpath
  relativePackages
  mergeOverrides
  concatOverrides
  normalizeOverrides
  overridesFor
  asFunction
  concatMapAttrsToList
  unwords
  unlines
  unlinesMap
  unlinesConcatMap
  modulesRaw
  foldAttrs
  foldMapAttrs
  over
  mergeAttr
  mergeAttrset
  mergeAuto
  mergeAll'
  mergeAll
  toTitle
  versionAtLeast
  minGhc
  empty
  evalModules
  overridesVia
  cabalDepPackage
  app
  loadConsole
  ;

  ghcOverlay = import ./ghc-overlay.nix;
}
