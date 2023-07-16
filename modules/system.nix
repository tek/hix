{ lib, config, ... }:
with lib;
let

in {
  options = with types; {

    system = mkOption {
      description = mdDoc "The system string like `x86_64-linux`, set by iterating over [](#opt-general-systems).";
      type = str;
      readOnly = true;
    };

  };
}
