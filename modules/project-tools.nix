{
  lib,
  config,
}:
haskell:
with builtins;
with lib;
let
  tags = import ../lib/tags.nix {
    inherit (config.inputs) thax;
    packages = config.internal.relativePackages;
    inherit (haskell) compiler pkgs ghc;
  };
  hpack = verbose: import ../lib/hpack.nix {
    inherit verbose;
    inherit (config.hpack) dir shared;
    inherit (haskell) pkgs;
    ghc = config.internal.basicGhc;
    paths = config.internal.relativePackages;
  };
in
  haskell // {
    inherit tags;
    hpack = { verbose ? false }: hpack verbose;
  }
