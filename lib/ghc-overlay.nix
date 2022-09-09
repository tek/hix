{
  compiler,
  rev,
  name,
  overrides ? {},
  overrideKeys ? ["local" "all" compiler "dev"],
}:
final: prev:
let

  o = import ./overrides.nix { inherit (prev) lib; };

  packages = o.ghcWithNamedOverrides overrideKeys overrides prev.haskell.packages.${compiler};

in {
  hixPackages =
    packages // { hix-nixpkgs-rev = rev; hix-name = name; };
}
