{lib}: let

  mergeOverrides = lib.zipAttrsWith (_: lib.concatLists);

  concatOverrides = lib.foldl (a: b: lib.toList a ++ lib.toList b) [];

  normalizeOverrides = project: deps: depsFull:
  let
    local = if lib.isAttrs project then project else { all = project; dev = project; };
    norm = lib.mapAttrs (_: o: if lib.isList o then o else [o]) local;
    depsOverrides = map (o: o.overrides // { local = []; }) deps;
    depsFullOverrides = map (o: o.overrides) depsFull;
  in { all = []; } // mergeOverrides (depsOverrides ++ depsFullOverrides ++ [norm]);

  overridesVia = desc: o:
  if desc == null
  then o
  else if lib.isAttrs o && o ? __source && o ? __functor
  then { __source = [desc] ++ o.__source; inherit (o) __functor; }
  else if lib.isFunction o
  then { __source = [desc]; __functor = _: o; }
  else if lib.isList o
  then map (overridesVia desc) o
  else if lib.isAttrs o
  then lib.mapAttrs (_: overridesVia) o
  else o
  ;

in {

  inherit
  mergeOverrides
  concatOverrides
  normalizeOverrides
  overridesVia
  ;

}
