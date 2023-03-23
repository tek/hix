{global, ...}:
{config, lib, ...}:
with lib;
{
  options = with types; {

    name = mkOption {
      type = str;
    };

    port = mkOption {
      type = port;
    };

    creds = mkOption {
      type = submodule {
        options = {
          user = mkOption {
            type = str;
          };
          password = mkOption {
            type = str;
          };
        };

        config = {
          user = mkDefault config.name;
          password = mkDefault config.name;
        };
      };
      default = {};
    };

    log = mkEnableOption "logging";

  };
}
