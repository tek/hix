{global, lib}:
{config, ...}: let
  inherit (lib) types;
in {

  options = {

    enable = lib.mkEnableOption "this service";

    config = lib.mkOption {
      description = "";
      type = types.unspecified;
      default = {};
    };

  };

}
