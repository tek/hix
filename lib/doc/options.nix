{ pkgs }:
with pkgs.lib;
let
  revision = "default";
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

  # TODO declarations appears to be always empty, so no links are generated
  optionsDoc = exclude: options: pkgs.nixosOptionsDoc {
    inherit options revision;
    allowDocBook = false;
    documentType = "none";
    transformOptions = opt: opt // { declarations = map (removePrefix "${toString ../..}/") opt.declarations; } // {
      visible = opt.visible && !(excludedByAny opt.loc exclude);
    };
  };

  json = exclude: options: (optionsDoc exclude options).optionsJSON;

  moduleDir = ../../modules;

  nameModule = { _module.args.name = mkOptionDefault namePh; };

  moduleWithout = exclude: name: args: let
    mod = import (moduleDir + "/${name}.nix") args;
    result = evalModules { modules = [mod nameModule]; };
  in json exclude result.options;

  module = name: args: moduleWithout [] name args;

in {
  inherit module moduleWithout revision namePh;
}
