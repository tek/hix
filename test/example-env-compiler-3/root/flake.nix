{
  description = "hix test project";
  inputs.hix.url = "HIX";
  outputs = {hix, ...}: hix [
    (import ./config.nix)
    ({config, ...}: {
      overrides = {noprofiling, noshared, ...}: {
        __all = noprofiling noshared;
      };
      packages.root.src = ./.;
    })
  ];
}
