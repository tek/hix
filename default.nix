inputs:
with builtins;
with inputs.nixpkgs.lib;
let
  inherit (inputs.nixpkgs) lib;

  compat = import ./lib/compat.nix { inherit lib; };

  api = makeExtensible (self: {
    lib = import ./lib/default.nix { inherit (inputs.nixpkgs) lib; };

    modules = projectModules: import ./modules/all-modules.nix {
      inherit inputs;
      projectModules = map compat.check (toList projectModules);
    };

    flake = projectModules:
    import ./modules/build.nix { inherit (inputs.nixpkgs) lib; modules = self.modules projectModules; };

    auto = projectModules:
    self.flake ([{ auto = true; ifd = false; }] ++ toList projectModules);

    pro = projectModules:
    self.flake ([(import ./modules/pro.nix)] ++ toList projectModules);

    overrides = import ./lib/overrides.nix { inherit (inputs.nixpkgs) lib; };

    spec = import ./lib/deps/spec.nix { inherit (self) lib; };
  });

  localOutputs = import ./lib/local.nix { inherit inputs; hix = api; };

in localOutputs // {
  lib = api;
}
