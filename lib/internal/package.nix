{util}: let

  inherit (util) lib;

  # Null check because `withPackage` and `mapMaybe` permit names that don't correspond to packages, since these
  # functions are used on sets that operate on a more generic level.
  isExposed = purpose: pkg: outputs: let
    expose = outputs.expose or pkg.expose or {};
  in
  if builtins.isBool expose
  then expose
  else expose.${purpose} or true;

  justExposed = purpose: pkg: outputs:
  if isExposed purpose pkg outputs then outputs else null;

  executables = pkg:
  lib.filterAttrs (_: e: e.enable) ({ __main = pkg.executable; } // pkg.executables);

  mainExe = pkg: let
    all = lib.attrValues (executables pkg);
  in
  if pkg.executable.enable
  then pkg.executable
  else if lib.length all > 0
  then lib.head all
  else null;

  withExe = alt: f: pkg: let
    exe = mainExe pkg;
  in util.maybe alt f exe;

  setWithExe = withExe {};

in {
  inherit
  isExposed
  justExposed
  mainExe
  withExe
  setWithExe
  ;
}
