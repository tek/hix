{lib, ...}: let
  inherit (lib) types;
in {
  options = {
    inputs = lib.mkOption {
      description = "The inputs of the Hix flake.";
      type = types.lazyAttrsOf types.unspecified;
      readOnly = true;
    };
  };
}
