{
  description = "hix test project";

  inputs.hix.url = path:HIX;
  inputs.dep.url = path:BASE/dep;

  outputs = { hix, dep, ... }: hix.lib.flake ({config, lib, ...}: {
    systems = ["x86_64-linux" "aarch64-darwin"];
    depsFull = [dep];
    packages.root = {
      src = ./.;
      library.enable = true;
      cabal.dependencies = ["aeson" "extra" "dep"];
    };
    compat.enable = false;
    overrides = {hackage, jailbreak, ...}: {
      aeson = hackage "2.1.2.1" "1f1f6h2r60ghz4p1ddi6wnq6z3i07j60sgm77hx2rvmncz4vizp0";
      extra = jailbreak;
    };
    gen-overrides.enable = true;
  });
}
