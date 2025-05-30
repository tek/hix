{util}: let

  packageDb = import ./package-db.nix { inherit util; };
  build = import ./ghc-build.nix { inherit util; };

in {
  inherit packageDb build;
}
