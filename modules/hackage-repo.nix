{util}:
{name, lib, ...}: let

  inherit (lib) types;

in {

  options = {

    name = lib.mkOption {
      description = ''
      Name used to refer to this server in other components.
      Uses the attribute name and cannot be changed.
      '';
      type = types.str;
      readOnly = true;
    };

    enable = lib.mkOption {
      description = "Whether to enable this Hackage server.";
      type = types.bool;
      default = true;
    };

    description = lib.mkOption {
      description = "Arbitrary short text for presentation, like 'local Hackage'.";
      type = types.nullOr types.str;
      default = null;
    };

    location = lib.mkOption {
      description = "Server URL with scheme and optional port.";
      type = types.str;
      example = "https://company-hackage.com:8080";
      # TODO tls?
      default = "https://hackage.haskell.org";
    };

    user = lib.mkOption {
      description = "User name for uploading.";
      type = types.nullOr types.str;
      default = null;
    };

    password = lib.mkOption {
      description = "Password for uploading.";
      type = types.nullOr util.types.password;
      default = null;
    };

    token = util.maybeOption types.str {
      description = ''
      Authentication token for uploading.
      '';
    };

    secure = lib.mkOption {
      description = "Use the newer Cabal client that verifies index signatures via `hackage-security`.";
      type = types.nullOr types.bool;
      default = true;
    };

    keys = lib.mkOption {
      description = "Security keys for this server.";
      type = types.nullOr (types.nonEmptyListOf types.str);
      default = null;
    };

    indexState = lib.mkOption {
      description = "When resolving, use the index at this time.";
      type = types.nullOr types.str;
      example = "2024-01-01T00:00:00Z";
      default = null;
    };

    solver = lib.mkOption {
      description = "Use this server for the Cabal resolver when managing dependency versions.";
      type = types.bool;
      default = true;
    };

    publish = lib.mkOption {
      description = "Publish packages to this server.";
      type = types.bool;
      default = false;
    };

  };

  config.name = name;

}
