{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = {self, hix, ...}: hix.lib._hix_test {
    managed.enable = true;
    packages.root.src = ./.;
  };
}
