{util}: let

  # TODO is this used?
  haskellModules = pkgs: "${pkgs.path}/pkgs/development/haskell-modules";

  build = import ./ghc-build.nix { inherit util; };

in {
  inherit haskellModules build;
}
