inputs:
with builtins;
with inputs.nixpkgs.lib;
let
  inherit (inputs.nixpkgs) lib;

  compat = import ./lib/compat.nix { inherit lib; };

  api = makeExtensible (self: {
    lib = import ./lib/default.nix { inherit (inputs.nixpkgs) lib; };

    modules = {projectModules ? [], extraModules ? []}: import ./modules/all-modules.nix {
      inherit inputs;
      projectModules = extraModules ++ map compat.check (toList projectModules);
    };

    flakeWith = modules:
    import ./modules/build.nix { inherit (inputs.nixpkgs) lib; modules = self.modules modules; };

    flake = projectModules: self.flakeWith { inherit projectModules; };

    auto = projectModules:
    self.flakeWith { extraModules = [{ auto = true; ifd = false; }]; modules = toList projectModules; };

    pro = projectModules:
    self.flakeWith { extraModules = [(import ./modules/pro.nix)]; projectModules = toList projectModules; };

    spec = import ./lib/deps/spec.nix { inherit (self) lib; };
  });

  localOutputs = import ./lib/local.nix { inherit inputs; hix = api; };

in localOutputs // {
  lib = api;
}
