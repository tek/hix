{
  lib,
  config,
}:
baseGhc:
with builtins;
with lib;
let
  tags = import ../lib/tags.nix {
    inherit (config.inputs) thax;
    packages = config.internal.relativePackages;
    inherit (baseGhc) compiler pkgs ghc;
  };
in
  baseGhc // {
    inherit tags;
    hpack = { verbose ? false }: hpack verbose;
  }
