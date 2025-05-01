{
  nixpkgs.old = {
    source = {
      rev = "70bdadeb94ffc8806c0570eb5c2695ad29f0e421";
      hash = "sha256-LWvKHp7kGxk/GEtlrGYV68qIvPHkU9iToomNFGagixU=";
    };
    config.allowBroken = true;
  };
  ghcVersions = ["ghc90"];
  envs.ghc90.package-set.compiler.nixpkgs = "old";
}
