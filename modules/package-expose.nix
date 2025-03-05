{util}: let

  inherit (util.lib) mkOption types;

in
{...}: {

  options = {

    target = mkOption {
      description = ''
      Whether to include this package in the default for [](#opt-env-packages).
      Disabling this simplifies configuring a package that can only be built by a specialized env, e.g. because it needs
      complicated overrides that should not infect other packages.
      '';
      type = types.bool;
      default = true;
    };

  };

}
