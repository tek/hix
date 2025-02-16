{
  description = "hix test project";

  inputs.hix.url = "path:HIX";
  inputs.dep.url = "path:BASE/dep";

  outputs = { hix, dep, ... }: hix.lib._hix_test {
    depsFull = [dep];
    ghcVersions = [];

    envs.profiled.enable = false;
    envs.min.enable = false;

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
    gen-overrides.enable = true;
    overrides = {self, pkgs, hackage, revision, jailbreak, buildInputs, minimal, ...}: {
      aeson = minimal (jailbreak (revision 1 "sha256-rA0N/BBUwCGJGjQnnkdPgAkIchZ2IpbufWV3+LCnb6I=" (
        revision 2 "invalid" (hackage "2.2.2.0" "sha256-1wVhkorow5G69wC3FC7nyhYRJVsAAhJkzB/pRn8GmYo="))));
      extra = jailbreak;
      root1 = jailbreak self.root;
      root = buildInputs [pkgs.git];
    };
    output.extraPackages = ["root1"];
  };
}
