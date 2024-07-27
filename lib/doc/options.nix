{pkgs}:
with pkgs.lib;
let
  namePh = "<name>";

  excluded = loc: { type, path, except ? [] }: let
    plen = length path;
    llen = length loc;
    isPrefix = take plen loc == path;
    properPrefix = plen < llen && isPrefix;
    suffix = drop plen loc;
    isMatch = path == loc;
    notExempt = !(any (e: suffix == e) except);
  in
  if type == "sub"
  then properPrefix && notExempt
  else if type == "eq"
  then isMatch
  else if type == "full"
  then isPrefix
  else error "invalid option exclude type: ${type}";

  excludedByAny = loc: any (excluded loc);

  excludeGlobal = any (seg: seg == "_module");

  # TODO declarations appears to be always empty, so no links are generated
  optionsDoc = exclude: options: pkgs.nixosOptionsDoc {
    inherit options;
    revision = "default";
    allowDocBook = false;
    documentType = "none";
    transformOptions = opt: opt // { declarations = map (removePrefix "${toString ../..}/") opt.declarations; } // {
      visible = opt.visible && !(excludeGlobal opt.loc) && !(excludedByAny opt.loc exclude);
    };
  };

  json = exclude: options: (optionsDoc exclude options).optionsJSON;

  moduleDir = ../../modules;

  nameModule = { _module.args.name = mkOptionDefault namePh; };

  modulesWithout = exclude: modules: let
    result = evalModules { modules = modules ++ [nameModule]; };
  in json exclude result.options;

  importMod = name: import (moduleDir + "/${name}.nix");

  moduleWithout = exclude: name: args: let
    mod = importMod name args;
  in modulesWithout exclude [mod];

  module = name: args: moduleWithout [] name args;

in {
  inherit importMod module moduleWithout modulesWithout namePh;
}
