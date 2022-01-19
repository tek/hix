{ lib, config, ... }:
with builtins;
with lib;
with types;
let

in {
  options = {

    hpack = {

      dir = mkOption {
        type = str;
        default = "ops/hpack";
      };

      shared = mkOption {
        type = str;
        default = "shared";
      };

    };
  };

}
