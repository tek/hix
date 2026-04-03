{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = { hix, ... }:
  hix.lib._hix_test ({config, ...}: {
    packages.root.src = ./.;

    hackage.versionFile = "./version";

    ui.warnings.keys."deprecated.option.hackage.versionFile" = false;
  });
}
