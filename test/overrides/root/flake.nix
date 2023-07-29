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
      test = {
        enable = true;
        dependencies = ["typed-process"];
      };
    };
    compat.enable = false;
    overrides = {self, pkgs, hackage, jailbreak, buildInputs, ...}: {
      aeson = hackage "2.1.2.1" "1f1f6h2r60ghz4p1ddi6wnq6z3i07j60sgm77hx2rvmncz4vizp0";
      extra = jailbreak;
      root1 = jailbreak self.root;
      root = buildInputs [pkgs.git];
    };
    gen-overrides.enable = true;
  });
}
