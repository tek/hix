{global, util}:
{
  pkgName,
  src,
  sort,
  desc,
  suffix,
  single,
}:
{
  name,
  lib,
  config,
  ...
}:
with lib;
let

  enableDesc = if single then "the ${desc} for this package" else "this ${desc}";

  suff = if suffix == null then "" else config.${suffix};

in {

  options = with types; {

    enable = mkEnableOption (enableDesc) // { default = !single; };

    name = mkOption {
      description =
        "The name of the ${desc}, defaulting to the attribute name in the config or the package name.";
      type = str;
      default = if single then "${pkgName}${suff}" else name;
    };

    source-dirs = mkOption {
      description = "Directories with Haskell sources.";
      type = either str (listOf str);
      default = if single then src else name;
    };

    env = mkOption {
      description = "The environment used when running GHCi with a module from this component.";
      type = nullOr util.types.env;
      default = null;
    };

    internal = {

      sort = mkOption {
        description = "Sort of the component (test, executable etc)";
        type = util.types.componentSort;
        default = sort;
        readOnly = true;
      };

      single = mkOption {
        description = ''
        Whether this is the main component of its sort, declared as `test` rather than `tests.foo`.
        '';
        type = bool;
        default = single;
        readOnly = true;
      };

    };

  };

}
