{
  description = "hix test project";
  inputs.hix.url = "github:tek/hix?ref=0.9.1";
  outputs = {hix, ...}: hix ({config, ...}: [import ./config.nix {
    overrides = {noprofiling, noshared, ...}: {
      __all = noprofiling noshared;
    };
    packages.root.src = ./.;
  }]);
}
