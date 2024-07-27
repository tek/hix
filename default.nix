inputs:
with builtins;
with inputs.nixpkgs.lib;
let
  api = makeExtensible (self: {
    lib = import ./lib/default.nix { inherit (inputs.nixpkgs) lib; };

    hixModules = import ./modules/all-modules.nix { inherit inputs; };

    flakeWith = {projectModules ? [], extraModules ? []}:
    import ./lib/eval.nix {
      inherit (inputs.nixpkgs) lib;
      inherit (self) hixModules;
      inherit projectModules extraModules;
    };

    flake = projectModules: self.flakeWith { projectModules = toList projectModules; };

    pro = projectModules:
    self.flakeWith { extraModules = [(import ./modules/pro.nix)]; projectModules = toList projectModules; };

    spec = import ./lib/deps/spec.nix { inherit (self) lib; };
  });

  testConfig = import ./ops/test-config.nix;
  localModule = import ./local.nix { inherit inputs; };
  localOutputs = api.pro [localModule testConfig];

in localOutputs // {
  lib = api;

  __functor = self: self.lib.flake;
}
