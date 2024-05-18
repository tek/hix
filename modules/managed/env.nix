{global, sort, util}:
{config, ...}: let

  inherit (util) lib;
  inherit (lib) mkOption mkDefault types;

  common = sort == "common";

  default = name: value:
    mkDefault (if common then value else global.managed.envs.${name});

  desc =
    if common
    then "managed dependencies"
    else
    if sort == "latest"
    then "managed latest versions"
    else "managed lower bounds";

in {

  options = {

      verbatim = mkOption {
        description = ''
        Default config for environments generated for managed dependencies.
        These can be overriden per-environment by specifying `envs.*.<attr>` like for any other environment.
        '';
        type = types.unspecified;
      };

      solverOverrides = mkOption {
        description = ''
        [Dependency overrides](#overrides-combinators) for the package set used only by the solver while finding new
        versions.
        Specifying these should only be necessary if the vanilla package set contains broken packages that would prevent
        the managed apps from starting.
        '';
        type = util.types.cabalOverridesVia desc;
      };

  };

  config = {

    verbatim = default "verbatim" {
      managed = mkDefault true;
      hide = mkDefault true;
    };

    solverOverrides = default "solverOverrides" [];

  };

}
