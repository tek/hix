{ lib, config, ... }:
with lib;
{
  options.packageSet = mkOption {
    type = types.unspecified;
  };

  config.packageSet = mkDefault rec {
    inherit (config.ghc) compiler nixpkgs pkgs ghc;
    inherit (config) packages base;
  };
}
