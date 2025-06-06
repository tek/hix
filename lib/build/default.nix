{util}: let

  targets = import ./targets.nix { inherit util; };

in {
  commands = import ./commands.nix { inherit util; };
  compilers = import ./compilers.nix { inherit util; };
  envs = import ./envs.nix { inherit util; };
  hpack = import ./hpack.nix { inherit util; };
  nixpkgs = import ./nixpkgs.nix { inherit util; };
  packages = import ./packages.nix { inherit util; };
  package-sets = import ./package-sets.nix { inherit util; };
} // targets
