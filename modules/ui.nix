{ lib, ... }: let

  inherit (lib) types;

in {

  options = {

    ui.warnings = {

      all = lib.mkOption {
        description = ''
        Whether to enable all warnings.
        Set this to `false` to suppress all warnings and use [](#opt-ui-ui.warnings.keys) to enable them
        selectively.
        '';
        type = types.bool;
        default = true;
      };

      keys = lib.mkOption {
        description = ''
        The set of warnings that should be enabled or disabled individually.
        Keys are warning IDs and values are booleans, where `true` enables a warning and `false` disables it.
        If a key is not present in this set, the warning is printed if [](#opt-ui-ui.warnings.all) is `true`.
        '';
        type = types.attrsOf types.bool;
        default = {};
      };

    };

    ui.experimental = {

      managed-maint = lib.mkOption {
        description = ''
        Whether to enable the experimental app `maint`, which attempts to adapt dependency bounds to new versions and
        publish Hackage revisions on success.
        '';
        type = types.bool;
        default = false;
      };

    };

  };

}
