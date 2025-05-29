{lib, ...}:
let
  inherit (lib) types;
  util = import ../lib/default.nix { inherit lib; };
in {

  options.systems = lib.mkOption {
    type = types.listOf types.str;
    description = "The systems for which to create outputs.";
    default = util.flake-utils.defaultSystems;
  };

}
