{lib, ...}:
with lib;
with types;
let
  util = import ../lib/default.nix { inherit lib; };
in {

  options.systems = mkOption {
    type = listOf str;
    description = mdDoc "The systems for which to create outputs.";
    default = util.flake-utils.defaultSystems;
  };

}
