{util, ...}:
{
  path ? ""
}:
with util.lib;
let

  inherit (util) pkgs;
  inherit (util.console) color indent;

  mods = util.evaledModules;

  pathSegs = if path == "" then [] else splitString "." path;

  colors = {
    attrset = "33";
    submodule = "32";
    option = "34";
  };

  desc = n: t: color n t + ": ";

  kvWith = col: name: value:
  [(desc col name + value)];

  kv =
  kvWith colors.option;

  sub = n:
  kv n "";

  zoom = segs: root: let
    get = p: f: config: let
      inner =
        if hasAttr p config
        then f config.${p}
        else throw ''
        No such config option: ${path}
        ${p} is not present in ${concatStringsSep ", " (attrNames config)}
        '';
    in
    { ${p} = inner; __zoom = true; };
  in foldr get id segs root;

  listOrEmpty = f: cs: n:
  if cs == []
  then kv n "[]"
  else sub n ++ indent (concatMap f cs);

  attrsOrEmpty = f: cs: n:
  if cs == {}
  then kv n "{}"
  else sub n ++ indent (util.concatMapAttrsToList f cs);

  renderPackage = p:
    if p ? pname
    then "${p.pname}-${p.version}"
    else p.name or "package";

  ghcDesc = g:
    if hasAttr "hix-name" g
    then " (Overrides for ${g.hix-name})"
    else "";

  renderers = {
    bool = b: if b then "true" else "false";
    str = s: ''"${s}"'';
    path = toString;
    separatedString = s: ''"${s}"'';
    unsignedInt16 = toString;
    package = renderPackage;
    nixpkgs = n: "nixpkgs source (${n.rev or "?"})";
    pkgs = _: "nixpkgs attrset";
    overlay = _: "overlay";
    cabal-overrides = _: "Cabal overrides";
    ghc = g: "Packages for GHC ${g.ghc.version}${ghcDesc g}";
  };

  stringifyStrict = c: splitString "\n" (generators.toPretty {} c);

  stringifyHpackDep = c:
  if isAttrs c
  then stringifyStrict c
  else [c];

  multilineRenderers = {
    strict = stringifyStrict;
    hpack-dep = stringifyHpackDep;
  };

  stringifyMultilinePre = prefix: lines: let
    first = [(concatStrings ([prefix] ++ take 1 lines))];
    rest = drop 1 lines;
  in
  if length lines <= 1
  then first
  else first ++ indent rest;

  renderGeneric = tpe: _:
  "<${tpe}>";

  stringifySimpleOptionValue = c: tpe:
  (renderers.${tpe} or (renderGeneric tpe)) c;

  stringifyOptionValuePre = prefix: c: tpe: let
    rendered =
    if hasAttr tpe.name multilineRenderers
    then multilineRenderers.${tpe.name} c
    else [(stringifySimpleOptionValue c tpe.name)];
  in
  stringifyMultilinePre prefix rendered;

  stringifyOptionValue = c: n: tpe:
  stringifyOptionValuePre (desc colors.option n) c tpe;

  stringifySubmodule = c: n: opt:
  [(desc colors.submodule n)] ++
  indent (stringifyModule c (opt.getSubOptions []));

  stringifyElem = tpe: c:
  stringifyOptionValuePre (color 35 "* ") c tpe;

  stringifyListOf = cs: n: opt:
  listOrEmpty (stringifyElem opt.nestedTypes.elemType) cs n;

  stringifyAttrsOf = cs: n: opt: let
    tpe = opt.nestedTypes.elemType;
  in
  attrsOrEmpty (n: c: stringifyOption c n tpe) cs n;

  stringifyAny = n: a:
    if isDerivation a
    then "<derivation>"
    else if isAttrs a
    then stringifyAttrset a n
    else if isFunction a
    then kv n "<function>"
    else kv n (generators.toPretty {} a)
    ;

  stringifyAttrset = cs: n:
  attrsOrEmpty stringifyAny cs n;

  stringifyAttrsStrict = cs: n: _:
  stringifyAttrset cs n;

  stringifyEither = c: n: opt:
  if opt.nestedTypes.left.check c
  then stringifyOption c n opt.nestedTypes.left
  else stringifyOption c n opt.nestedTypes.right;

  nestedHandlers = {
    submodule = stringifySubmodule;
    listOf = stringifyListOf;
    attrsOf = stringifyAttrsOf;
    lazyAttrsOf = stringifyAttrsOf;
    attrs = stringifyAttrsStrict;
    either = stringifyEither;
  };

  stringifyOption = c: n: tpe: let
    handler = nestedHandlers.${tpe.name} or stringifyOptionValue;
  in handler c n tpe;

  stringifyAttrs = c: n: a:
  [(desc colors.attrset n)] ++
  (if isDerivation a
  then ["<derivation>"]
  else indent (stringifyModule c a));

  stringifyValue = c: n: zoomed: a:
  if ! zoomed && (n == "_module" || n == "internal" || n == "code" || n == "runner" || n == "devGhc")
  then []
  else if isAttrs a
  then
  if (a._type or "nothing") == "option"
  then stringifyOption c n a.type
  else stringifyAttrs c n a
  else stringifyAny n a;

  stringifyModule = c: opts:
  util.concatMapAttrsToList (n: a: optionals (n != "__zoom" && isAttrs c && hasAttr n c) (stringifyValue c.${n} n (c.__zoom or false) a)) opts;

  stringifyRoot = let
    lines = stringifyModule (zoom pathSegs mods.config) mods.options ++ [""];
  in pkgs.writeText "project-options" (util.unlines lines);

  palette = "Colors: ${concatStringsSep " | " (mapAttrsToList (flip color) colors)}";

in util.hixScript "show-config" {} ''
  hix_print "${palette}"
  hix_print ""
  while IFS='\n' read -r line
  do
    hix_print $line
  done < ${stringifyRoot}
''
