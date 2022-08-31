inputs:
with builtins;
with inputs.nixpkgs.lib;
let
  inherit (inputs.nixpkgs) lib;

  localOutputs =
    inputs.flake-utils.lib.eachSystem ["x86_64-linux"] (system: import ./lib/local.nix { inherit system inputs; });

  compat = import ./lib/compat.nix { inherit lib; };

  api = makeExtensible (self: {
    lib = import ./lib/default.nix { inherit (inputs.nixpkgs) lib; };

    modules = projectModules: import ./modules/all-modules.nix {
      inherit inputs;
      projectModules = toList projectModules;
    };

    flake = projectModules:
    import ./modules/build.nix { inherit (inputs.nixpkgs) lib; modules = self.modules projectModules; };

    auto = projectModules:
    self.flake ([{ auto = true; ifd = false; }] ++ toList projectModules);

    obeliskOverrides = import ./obelisk/overrides.nix { inherit (inputs) obelisk; };

    overrides = import ./lib/overrides.nix { inherit (inputs.nixpkgs) lib; };

    spec = import ./deps/spec.nix { inherit (self) lib; };
  });

in localOutputs // {
  lib = api;
}
