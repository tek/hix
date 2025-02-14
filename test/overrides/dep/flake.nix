{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }: hix.lib._hix_test {
    packages.dep = {
      src = ./.;
      library.enable = true;
      cabal.dependencies = ["some"];
    };
    overrides = {hackage, ...}: {
      some = hackage "1.0.6" "1afwapqlkhyp6mrd9qypbkj6hqppxi3sq4c5jjvvic556i9xly02";
    };
    gen-overrides.enable = true;
  };
}
