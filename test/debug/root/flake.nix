{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = {hix, ...}:
  hix.lib._hix_test {
    packages.root.src = ./.;
  };
}
