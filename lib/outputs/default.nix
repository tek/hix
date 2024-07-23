{util}: {
  commands = import ./commands.nix { inherit util; };
  envs = import ./envs.nix { inherit util; };
  hpack = import ./hpack.nix { inherit util; };
  managed = import ./managed.nix { inherit util; };
  packages = import ./packages.nix { inherit util; };
}
