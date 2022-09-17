{ config, lib, util, ... }:
{
  path ? null
}:
with builtins;
with lib;
let

  unlines = concatStringsSep "\n";

  indentLine = l: "  " + l;

  indent = map indentLine;

  concatMapAttrs = f: a: concatLists (mapAttrsToList f a);

  desc = n: t: "\\e[" + toString n + "m" + t + "\\e[0m: ";

  kv = n: v:
  [(desc 34 n + v)];

  sub = n:
  kv n "";

  mods = util.modulesRaw {} (util.modules ++ [{ system = config.system; } (import ../modules/system.nix)]);
  # mods = util.modulesRaw {} util.modules;

  options = mods.options;

  renderPackage = p:
    if p ? pname
    then "${p.pname}-${p.version}"
    else p.name or "package";

  renderers = {
    bool = b: if b then "true" else "false";
    str = s: ''\"${s}\"'';
    path = toString;
    separatedString = s: ''\"${s}\"'';
    unsignedInt16 = toString;
    package = renderPackage;
    nixpkgs = n: "nixpkgs source (${n.rev or "?"})";
    pkgs = _: "nixpkgs attrset";
    overlay = _: "overlay";
    cabalOverrides = _: "Cabal overrides";
  };

  renderGeneric = tpe: _:
  "<${tpe}>";

  stringifyOptionValue = c: tpe:
  (renderers.${tpe} or (renderGeneric tpe)) c;

  stringifySubmodule = c: n: a:
  [(desc 33 n)] ++
  indent (stringifyModule c (a.getSubOptions []));

  stringifyElem = tpe: c:
  "- " + stringifyOptionValue c tpe;

  stringifyListOf = c: n: a:
  sub n ++
  indent (map (stringifyElem a.nestedTypes.elemType.name) c);

  stringifyAttrOf = tpe: n: c:
  kv n (stringifyOptionValue c tpe);

  stringifyAttrsOf = cs: n: a: let
    tpe = a.nestedTypes.elemType;
  in
  sub n ++
  indent (concatMapAttrs (n: c: stringifyOption c n tpe) cs);

  stringifyAny = n: a:
    if isDerivation a
    # then "<derivation> ${toString (attrNames a)}"
    then "<derivation>"
    else if isAttrs a
    then stringifyAttrsStrict a n null
    else if isFunction a
    then kv n "<function>"
    else kv n (toString a)
    ;

  stringifyAttrsStrict = c: n: _:
  [(desc 34 n)] ++ indent (concatMapAttrs stringifyAny c);

  stringifyEither = c: n: a:
  if a.nestedTypes.left.check c
  then stringifyOption c n a.nestedTypes.left
  else stringifyOption c n a.nestedTypes.right;

  nestedHandlers = {
    submodule = stringifySubmodule;
    listOf = stringifyListOf;
    attrsOf = stringifyAttrsOf;
    lazyAttrsOf = stringifyAttrsOf;
    attrs = stringifyAttrsStrict;
    either = stringifyEither;
  };

  stringifyOption = c: n: tpe:
  if hasAttr tpe.name nestedHandlers
  then nestedHandlers.${tpe.name} c n tpe
  else kv n (stringifyOptionValue c tpe.name);

  stringifyOther = c: n: a:
  [(desc 35 n + "other")];

  stringifyAttrs = c: n: a:
  [(desc 33 n)] ++
  (if isDerivation a
  then ["derivation"]
  else indent (concatMapAttrs (n: stringifyAttr c n) a));

  stringifyAttr = c: n: a:
  if n == "_module"
  then []
  else if isAttrs a
  then
  if (a._type or "nothing") == "option"
  then stringifyOption c.${n} n a.type
  else stringifyAttrs c.${n} n a
  else
  stringifyOther c.${n} n a;

  stringifyModule = c: m: concatMapAttrs (stringifyAttr c) m;

  stringifyRoot = r: unlines (stringifyModule config r);

in config.pkgs.writeScript "show-config" ''
  #!${config.pkgs.zsh}/bin/zsh
  echo "${stringifyRoot options}"
''
