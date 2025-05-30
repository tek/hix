{util}: {

  command = import ./command.nix { inherit util; };
  env = import ./env.nix { inherit util; };
  envs = import ./envs.nix { inherit util; };
  ghc = import ./ghc.nix { inherit util; };
  hpack = import ./hpack.nix { inherit util; };
  modules = import ./modules.nix { inherit util; };
  overrides = import ./overrides.nix { inherit util; };
  package = import ./package.nix { inherit util; };
  packages = import ./packages.nix { inherit util; };
  package-sets = import ./package-sets.nix { inherit util; };
  path = import ./path.nix { inherit util; };
  project = import ./project.nix { inherit util; };
  warn = import ./warn.nix { inherit util; };
  vm = import ./vm.nix { inherit util; };

}
