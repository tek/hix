{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }: hix.lib.flake {
    packages.dep = {
      src = ./.;
      library.enable = true;
      cabal.dependencies = ["some"];
    };
    overrides = {hackage, ...}: {
      some = hackage "1.0.5" "1kngk5p1nxcd69m2kma75ym61w6l09pa7260pg8vsq44cwf35ip0";
    };
    gen-overrides.enable = true;
  };
}
