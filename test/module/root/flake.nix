{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = {hix, ...}:
  hix.lib._hix_test [
    ({util, lib, ...}: {
      packages.root.src = ./.;
      outputs.legacyPackages.a = util.fromMaybeNull 3 3;
      name = "a";
    })
    ./config-b.nix
    ./config-c.nix
  ];
}
