{global, ...}:
{name, config, lib, ...}:
with lib;
let

  serviceModule = import ./service.nix { inherit lib global; };

in {
  options = with types; {

    enable = mkEnableOption (mdDoc "this shell");

    name = mkOption {
      description = mdDoc "Shell name";
      type = str;
      default = name;
    };

    buildInputs = mkOption {
      description = "";
      type = listOf package;
      default = {};
    };

    shellHook = mkOption {
      description = "";
      type = str;
      default = {};
    };

    derivation = mkOption {
      type = package;
      readOnly = true;
    };

  };

  config = {

    enable = mkDefault true;

    exit = mkDefault exitServices;

    derivation = mkDefault (global.pkgs.stdenv.mkDerivation { inherit (config) name buildInputs shellHook; });

  };
}
