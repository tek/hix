{global, lib}:
with lib;
{config, ...}:
{

  options = with types; {

    enable = mkEnableOption (mdDoc "this service");

    config = mkOption {
      description = "";
      type = unspecified;
      default = {};
    };

  };

}
