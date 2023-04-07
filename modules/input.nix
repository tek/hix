{ config, lib, ... }:
with lib;
{
  options = with types; {
    system = mkOption {
      description = mdDoc "This option is set dynamically for each configured system.";
      type = str;
      readOnly = true;
    };

    inputs = mkOption {
      description = mdDoc "The inputs of the Hix flake.";
      type = lazyAttrsOf unspecified;
      readOnly = true;
    };
  };
}
