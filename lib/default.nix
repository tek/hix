{lib}:
let

  inherit (lib) mapAttrs isAttrs isList concatStringsSep attrNames isDerivation length head tail filterAttrs;

  internalScope = "__hix-internal__";

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

  mergeOverrides = lib.zipAttrsWith (_: lib.concatLists);

  concatOverrides = lib.foldl (a: b: lib.toList a ++ lib.toList b) [];

  normalizeOverrides = project: deps: depsFull:
  let
    local = if isAttrs project then project else { all = project; dev = project; };
    norm = mapAttrs (_: o: if isList o then o else [o]) local;
    depsOverrides = map (o: o.overrides // { local = []; }) deps;
    depsFullOverrides = map (o: o.overrides) depsFull;
  in { all = []; } // mergeOverrides (depsOverrides ++ depsFullOverrides ++ [norm]);

  concatMapAttrsToList = f: a: lib.concatLists (lib.mapAttrsToList f a);

  unwords = concatStringsSep " ";

  unlines = concatStringsSep "\n";

  unlinesMap = lib.concatMapStringsSep "\n";

  unlinesConcatMap = f: xs: concatStringsSep "\n" (lib.concatMap f xs);

  # catSets :: [Attrs a] -> Attrs a
  catSets = lib.mergeAttrsList;

  mapListCatAttrs = f: xs:
  catSets (map f xs);

  catValues = a:
  catSets (lib.attrValues a);

  foldMapAttrs = f: set:
  lib.foldlAttrs (z: name: attr: z // f name attr) {} set;

  over = path: f: attrs:
  if lib.hasAttrByPath path attrs
  then lib.updateManyAttrsByPath [{ inherit path; update = f; }] attrs
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
    Derivation names: ${a.pname or a.name or "<none>"} / ${b.pname or b.name or "<none>"}
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
    if lib.hasAttr name l && lib.hasAttr name r
    then mergeAttr name l.${name} r.${name}
    else l.${name} or r.${name};
  in lib.genAttrs (lib.concatMap attrNames [l r]) f;

  mergeAuto = a: b:
  if b == null
  then a
  else if isList a
  then a ++ b
  else if isAttrs a
  then mergeAttrset a b
  else b;

  mergeAll' = z: items:
  lib.foldl mergeAuto z items;

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
  if lib.stringLength s == 0
  then s
  else lib.toUpper (lib.substring 0 1 s) + lib.substring 1 (-1) s;

  minGhc = version: env:
  lib.versionAtLeast env.ghc.version version;

  empty = v: if isAttrs v then empty (attrNames v) else v == [];

  evalModules = modules: lib.evalModules { inherit modules; };

  evalConfig = modules: (evalModules modules).config;

  overridesVia = desc: o:
  if desc == null
  then o
  else if isAttrs o && o ? __source && o ? __functor
  then { __source = [desc] ++ o.__source; inherit (o) __functor; }
  else if lib.isFunction o
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

  app = program: let
    main =
      if program ? meta.mainProgram
      then "${program}/bin/${program.meta.mainProgram}"
      else "${program}";
  in { type = "app"; program = main; meta = {}; };

  removeApp = a: removeAttrs a ["program" "type"];

  console = import ./console.nix { inherit lib; };

  messageStream = ">& $_hix_msg_fd";

  loadConsoleWith = {verbose ? true}: let

    inherit (console) colors aliases chevrons chevronY chevronM chevronsH startSgr resetSgr;

    colorFun = name: ''
    ${name}()
    {
      _hix_nest_sgr "${startSgr colors.${name}}" "$*"
    }
    '';

    startColor = name: ''
    ${name}_start="${startSgr colors.${name}}"
    '';

    colorAliasFun = alias: for: ''
    color_${alias}()
    {
      ${for} "$*"
    }
    '';

  in ''
  _hix_console_is_zsh()
  {
    [[ -n "''${ZSH_NAME:-}" ]]
  }

  hix_cat()
  {
    cat $* ${messageStream}
  }

  _hix_echo()
  {
    echo "$@" ${messageStream}
  }

  if _hix_console_is_zsh
  then
    hix_print_raw()
    {
      $=_hix_printer_raw "$@"
    }
    hix_print()
    {
      $=_hix_printer "$@"
    }
    _hix_redirect()
    {
      $=* ${messageStream}
    }
  else
    hix_print_raw()
    {
      $_hix_printer_raw "$@"
    }
    hix_print()
    {
      $_hix_printer "$@"
    }
    _hix_redirect()
    {
      $* ${messageStream}
    }
  fi

  export _hix_printer_raw=''${_hix_printer_raw:-_hix_echo}
  export _hix_printer=''${_hix_printer:-_hix_echo -e}
  export _hix_msg_fd=2
  export _hix_verbose=${if verbose then "1" else "0"}

  reset_sgr="${resetSgr}"
  bold_start="${startSgr 1}"
  chevronsH='${chevronsH}'
  chevrons='${chevrons}'
  chevronY='${chevronY}'
  chevronM='${chevronM}'

  ${unlines (map colorFun (attrNames colors))}
  ${unlines (map startColor (attrNames colors))}
  ${unlines (lib.mapAttrsToList colorAliasFun aliases)}

  # SGR 28 is "Reveal"
  _hix_color_marker=$(echo -e "${startSgr 28}")

  # Place a marker after the colored string, and replace it in substrings by the current SGR code to allow easy nesting
  # of colors.
  # The marker stays in to allow multiple levels (for bold + fg + bg), but since it is a no-op SGR itself that's fine.
  _hix_nest_sgr()
  {
    local sgr=$1
    local string=$2
    local nest="''${string//''${_hix_color_marker}/$_hix_color_marker$sgr}"
    echo -e "$sgr$nest${resetSgr}$_hix_color_marker"
  }

  _hix_sanitize_sgr()
  {
    echo "''${*//''${_hix_color_marker}/}"
  }

  bold()
  {
    _hix_nest_sgr "${startSgr 1}" "$*"
  }

  _hix_message_fragment()
  {
    hix_print -n "$(_hix_sanitize_sgr "$*")"
  }

  _hix_message()
  {
    hix_print "$(_hix_sanitize_sgr "$chevrons $*")"
  }

  _hix_message_part()
  {
    hix_print -n "$(_hix_sanitize_sgr "$chevrons $*")"
  }

  message_fragment()
  {
    if (( $_hix_verbose == 1 ))
    then
      _hix_message_fragment "$*"
    fi
  }

  message()
  {
    if (( $_hix_verbose == 1 ))
    then
      _hix_message "$*"
    fi
  }

  message_part()
  {
    if (( $_hix_verbose == 1 ))
    then
      _hix_message_part "$*"
    fi
  }

  message_hang()
  {
    message "  $*"
  }

  message_part_hang()
  {
    message_part "  $*"
  }

  error_message()
  {
    message "$(color_error $*)"
  }

  error_message_hang()
  {
    error_message "  $*"
  }

  error_message_part_hang()
  {
    _hix_message_part "  $*"
  }

  die()
  {
    error_message $*
    exit 1
  }

  if _hix_console_is_zsh
  then
    ask()
    {
      setopt local_options no_err_exit no_err_return
      local decision=""
      message_part "$1 [Yn] "
      read -k decision
      if [[ $decision != '\n' ]]
      then
        hix_print
      fi
      [[ $decision != 'n' ]]
    }
  fi
  '';

  loadConsole = loadConsoleWith {};

in {
  inherit
  lib
  internalScope
  utilWithConfig
  utilModule
  flake-utils
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
  minGhc
  empty
  evalModules
  evalConfig
  overridesVia
  cabalDepPackage
  app
  removeApp
  console
  messageStream
  loadConsoleWith
  loadConsole
  ;

  version = import ./version.nix { inherit lib; };
}
