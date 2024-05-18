{global, lib}:
with lib;
{config, ...}:
{

  options = with types; {

    enable = mkEnableOption ("this service");

    config = mkOption {
      description = "";
      type = unspecified;
      default = {};
    };

  };

}
