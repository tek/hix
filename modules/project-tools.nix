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
  hpack = verbose: import ../lib/hpack.nix {
    inherit verbose;
    inherit (config.hpack) packages;
    inherit (baseGhc) pkgs;
    ghc = config.internal.basicGhc;
    paths = config.internal.relativePackages;
  };
in
  baseGhc // {
    inherit tags;
    hpack = { verbose ? false }: hpack verbose;
  }
