{global, lib, util, ...}:
{config, ...}:
let

  inherit (lib) mkOption types;

  portModule = {config, ...}: {
    options = {

      guest = mkOption {
        description = "Port used in the VM.";
        type = types.port;
      };

      host = mkOption {
        description = ''
        Port exposed in the system, relative to the env's [](#opt-env-basePort) unless [](#opt-service-ports._name_.absolute)
        is set.
        '';
        type = types.port;
      };

      absolute = mkOption {
        description = ''
        Whether the host port is an absolute number. If `false` (default), the port is added to [](#opt-env-basePort).
        '';
        type = types.bool;
        default = false;
      };

    };

    config.host = lib.mkDefault config.guest;
  };

in {
  options = {

    enable = mkOption {
      description = "Enable this service";
      type = util.types.multiEnable;
      default = true;
    };

    nixos = mkOption {
      description = "NixOS config used for the service VM.";
      type = types.either types.deferredModule (types.listOf types.deferredModule);
      default = {};
    };

    nixos-base = mkOption {
      description = "NixOS base config used for the service VM.";
      type = types.either types.deferredModule (types.listOf types.deferredModule);
      default = {};
    };

    ports = mkOption {
      description = "Simple ports forwarded relative to the env's [](#opt-env-basePort).";
      type = types.attrsOf (types.submodule portModule);
      default = {};
    };

    messages = mkOption {
      description = "Informational messages that will be echoed when an environment starts this service.";
      type = types.functionTo (types.listOf types.str);
      default = _: [];
    };

  };
}
