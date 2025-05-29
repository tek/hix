{lib, ...}:
let
  inherit (lib) types;
in {
  options = {

    system = lib.mkOption {
      description = "The system string like `x86_64-linux`, set by iterating over [](#opt-general-systems).";
      type = types.str;
      readOnly = true;
    };

  };
}
