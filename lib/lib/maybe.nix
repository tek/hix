{self}: let

  inherit (self) lib;

  maybeNull = alt: f: a: if a == null then alt else f a;

  fromMaybeNull = alt: maybeNull alt lib.id;

  restrictKeys = allow: lib.filterAttrs (k: _: lib.elem k allow);

  removeKeys = lib.flip builtins.removeAttrs;

  pickNonNullAttrAs = rename: name: attrs: maybeNull {} (value: { ${rename} = value; }) attrs.${name};

  pickNonNullAttr = name: pickNonNullAttrAs name name;

  # data Maybe =
  #  Just { __maybe :: Bool, value :: a | __maybe == True }
  #  |
  #  Nothing { __maybe :: Bool | __maybe == False }
  just = value: { __maybe = true; inherit value; };
  nothing = { __maybe = false; };

  justIf = pred: value: if pred then just value else nothing;

  justNonNull = value: justIf (value != null) value;

  justAttr = name: attrs: justIf (attrs ? ${name}) attrs.${name};

  assertMaybe = ma: b:
  if ma ? __maybe
  then b
  else throw "Invalid maybe value: ${lib.generators.toPretty {} ma}";

  isJust = ma: assertMaybe ma ma.__maybe;

  maybe = alt: f: ma:
  assertMaybe ma (
    if ma.__maybe
    then f ma.value
    else alt
  );

  # fromMaybe :: a -> Maybe a -> a
  fromMaybe = alt: maybe alt lib.id;

  # expectJust :: String -> Maybe a -> a
  expectJust = err: fromMaybe (throw err);

  # catMaybes :: Attrs (Maybe a) -> Attrs a
  catMaybes = as: self.mapValues (ma: ma.value) (lib.filterAttrs (_: isJust) as);

  # mapMaybe :: (a -> Maybe b) -> Attrs a -> Attrs b
  mapMaybe = f: a: catMaybes (lib.mapAttrs f a);

  # apMaybe :: (a -> b) -> Maybe a -> Maybe b
  apMaybe = f: ma:
  if isJust ma
  then ma // { value = f ma.value; }
  else ma
  ;

  bindMaybe = f:
  maybe nothing f;

  # find :: (a -> Bool) -> [a] -> Maybe a
  find = pred: as: justNonNull (lib.findFirst pred null as);


in {

  inherit
  maybeNull
  fromMaybeNull
  restrictKeys
  removeKeys
  pickNonNullAttrAs
  pickNonNullAttr
  just
  nothing
  justIf
  justNonNull
  justAttr
  maybe
  fromMaybe
  expectJust
  catMaybes
  mapMaybe
  apMaybe
  bindMaybe
  find
  ;

}
