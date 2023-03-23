{global, lib, ...}:
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
        description = "Port exposed in the system, relative to the env's [](#opt-env-basePort).";
        type = port;
        default = config.guest;
      };

    };
  };

in {
  options = with types; {

    enable = mkEnableOption (mdDoc "this service");

    nixos = mkOption {
      description = "NixOS config used for the service VM.";
      type = unspecified;
      default = {};
    };

    nixos-base = mkOption {
      description = "NixOS base config used for the service VM.";
      type = unspecified;
      default = {};
    };

    ports = mkOption {
      description = mdDoc "Simple ports forwarded relative to the env's [](#opt-env-basePort).";
      type = listOf (submodule portModule);
      default = [];
    };

  };
}
