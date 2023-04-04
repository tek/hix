{ config, lib, ... }:
with lib;
{
  options = with types; {
    system = mkOption {
      description = mdDoc "This option is set dynamically for each configured system.";
      type = str;
    };

    inputs = mkOption {
      description = mdDoc "";
      type = lazyAttrsOf unspecified;
      default = {};
    };
  };
}
