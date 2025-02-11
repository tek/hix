{
  description = "hix test project";

  inputs.hix.url = "path:HIX";
  inputs.dep.url = "path:BASE/dep";

  outputs = { hix, dep, ... }: hix.lib._hix_test ({config, lib, ...}: {
    systems = ["x86_64-linux" "aarch64-darwin"];
    depsFull = [dep];
    ghcVersions = [];
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
      aeson = minimal (jailbreak (revision 2 "sha256-yE5ZRHn17Gd8qt42Pqou9Ln9xvrjPtBxxuuGAsrD05s=" (
        revision 3 "invalid" (hackage "2.1.2.1" "sha256-4P64yWe27iw6PKc+DYw8II5vsOUmthYu+fABkwU0Lrg="))));
      extra = jailbreak;
      root1 = jailbreak self.root;
      root = buildInputs [pkgs.git];
    };
    output.extraPackages = ["root1"];
  });
}
