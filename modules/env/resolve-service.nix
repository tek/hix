{util, env}: let

  inherit (util) lib;
  inherit (lib) types;

  global = util.config;

  serviceModule = import ../service.nix { inherit util; };

in {name, config, ...}: let
  service = env.services.${name};
in {
  options = {
    name = lib.mkOption {
      description = "Name of the service";
      type = types.str;
      default = name;
      readOnly = true;
    };

    resolve = lib.mkOption {
      description = "Service config assembled from various sources";
      type = types.submoduleWith {
        modules = lib.optionals (name != "‹name›") ([
          serviceModule
          { inherit (service) enable; }
        ] ++
        lib.optional (lib.hasAttr name global.services) global.services.${name} ++
        lib.optionals (lib.hasAttr name global.internal.services) [
          global.internal.services.${name}
          service.config
        ]);
      };
      default = {};
    };
  };
}
