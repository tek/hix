{global, util}:
{
  pkgName,
  src,
  sort,
  desc,
  suffixOption,
  single,
}:
{
  name,
  lib,
  config,
  ...
}:
let
  inherit (lib) types mkOption;

  enableDesc = if single then "the ${desc} for this package" else "this ${desc}";

  # If `suffixOption` is non-null, it is used as an option name to read the component suffix.
  # This is an option on a component, defined in `./cabal-options.nix`.
  # The component suffix is used to create the default name for a singleton component, which may be `hix-test` for
  # `packages.hix.test`.
  # Its values are `-test`, `-bench` etc, which are appended to the package name.
  # `single` is true for singleton components (`packages.*.test` vs. `packages.*.tests.integration`), so only those are
  # affected.
  # `suffixOption` is null for the main library, which has no name.
  nameSuffix = if suffixOption == null then "" else config.${suffixOption};

in {

  options = {

    enable = lib.mkEnableOption enableDesc // { default = !single; };

    name = mkOption {
      description =
        "The name of the ${desc}, defaulting to the attribute name in the config or the package name.";
      type = types.str;
      default = if single then "${pkgName}${nameSuffix}" else name;
    };

    source-dirs = mkOption {
      description = "Directories with Haskell sources.";
      type = types.either types.str (types.listOf types.str);
      default = if single then src else name;
    };

    env = mkOption {
      description = "The environment used when running GHCi with a module from this component.";
      type = types.nullOr util.types.env;
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
        type = types.bool;
        default = single;
        readOnly = true;
      };

    };

  };

}
