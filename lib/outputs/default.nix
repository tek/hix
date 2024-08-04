{util}: {
  commands = import ./commands.nix { inherit util; };
  envs = import ./envs.nix { inherit util; };
  hackage = import ./hackage.nix { inherit util; };
  hpack = import ./hpack.nix { inherit util; };
  internals = import ./internals.nix { inherit util; };
  managed = import ./managed.nix { inherit util; };
  packages = import ./packages.nix { inherit util; };
  cli-context = import ./cli-context.nix { inherit util; };
}
