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

    obeliskOverrides = import ./obelisk/overrides.nix { inherit (inputs) obelisk; };

    overrides = import ./lib/overrides.nix { inherit (inputs.nixpkgs) lib; };

    spec = import ./deps/spec.nix { inherit (self) lib; };
  });

  # TODO this uses a module config separate from the consumer project.
  # therefore, the systems are statically fixed to what is defined in hix (so the default).
  # should find a way to copy the systems. this seems impossible the way it is set up now.
  # maybe just add aarch64 and be done with it.
  localOutputs =
    # inputs.flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      import ./lib/local.nix { inherit inputs; hix = api; };
    # );

in localOutputs // {
  lib = api;
}
