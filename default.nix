inputs:
let
  inherit (inputs.nixpkgs) lib;

  api = lib.makeExtensible (self: {
    lib = import ./lib/default.nix { inherit lib; };

    hixModules = import ./modules/all-modules.nix { inherit inputs; };

    flakeWith = {projectModules ? [], extraModules ? []}:
    import ./lib/eval.nix {
      inherit (self) hixModules;
      inherit lib projectModules extraModules;
    };

    flake = projectModules: self.flakeWith { projectModules = lib.toList projectModules; };

    pro = projectModules:
    self.flakeWith { extraModules = [(import ./modules/pro.nix)]; projectModules = lib.toList projectModules; };

    _hix_test = projectModules:
    self.flakeWith { extraModules = [(import ./modules/hix-test.nix)]; projectModules = lib.toList projectModules; };

    spec = import ./lib/deps/spec.nix { inherit (self) lib; };
  });

  testConfig = import ./ops/test-config.nix;
  localModule = import ./local.nix { inherit inputs; };
  localOutputs = api.pro [localModule testConfig];

in localOutputs // {
  lib = api;

  __functor = self: self.lib.flake;
}
