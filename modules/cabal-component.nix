{
  pkgName,
  src,
  sort,
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

  enableDesc = if single then "the ${sort} for this package" else "this ${sort}";

  suff = if suffix == null then "" else config.${suffix};

in
{

  options = with types; {

    enable = mkEnableOption enableDesc // { default = !single; };

    name = mkOption {
      description = "The name of the ${sort}, defaulting to the attribute name in the config or the package name.";
      type = str;
      default = if single then pkgName + suff else name;
    };

    source-dirs = mkOption {
      type = either str (listOf str);
      description = "Directories with Haskell sources.";
      default = src;
    };

  };

}
