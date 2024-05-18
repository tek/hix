{global, lib, util, ...}:
{config, ...}:
with lib;
let

  portModule = {config, ...}: {
    options = with types; {

      guest = mkOption {
        description = "Port used in the VM.";
        type = port;
      };

      host = mkOption {
        description = ''
        Port exposed in the system, relative to the env's [](#opt-env-basePort) unless [](#opt-service-ports._name_.absolute)
        is set.
        '';
        type = port;
      };

      absolute = mkOption {
        description = ''
        Whether the host port is an absolute number. If `false` (default), the port is added to [](#opt-env-basePort).
        '';
        type = bool;
        default = false;
      };

    };

    config.host = mkDefault config.guest;
  };

in {
  options = with types; {

    enable = mkOption {
      description = "Enable this service";
      type = util.types.multiEnable;
      default = true;
    };

    nixos = mkOption {
      description = "NixOS config used for the service VM.";
      type = deferredModule;
      default = {};
    };

    nixos-base = mkOption {
      description = "NixOS base config used for the service VM.";
      type = deferredModule;
      default = {};
    };

    ports = mkOption {
      description = "Simple ports forwarded relative to the env's [](#opt-env-basePort).";
      type = attrsOf (submodule portModule);
      default = {};
    };

    messages = mkOption {
      description = "Informational messages that will be echoed when an environment starts this service.";
      type = functionTo (listOf str);
      default = _: [];
    };

  };
}
