{
  description = "hix test project";
  inputs.hix.url = "HIX";
  outputs = {hix, ...}: hix [
    (import ./config.nix)
    ({config, ...}: { packages.root.src = ./.; })
  ];
}
