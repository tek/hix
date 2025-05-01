{
  description = "hix test project";
  inputs.hix.url = "github:tek/hix?ref=0.9.1";
  outputs = {hix, ...}: hix ({config, ...}: [import ./config.nix { packages.root.src = ./.; }]);
}
