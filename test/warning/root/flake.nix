{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }:
  hix.lib._hix_test ({config, ...}: {
    packages.root.src = ./.;

    outputs.legacyPackages = {
      package-subpath = config.packages.root.subpath;
    };

  });
}
