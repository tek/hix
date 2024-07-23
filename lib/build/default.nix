{util}: let

  targets = import ./targets.nix { inherit util; };

in {
  commands = import ./commands.nix { inherit util; };
  envs = import ./envs.nix { inherit util; };
  packages = import ./packages.nix { inherit util; };
} // targets
