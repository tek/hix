inputs:
with builtins;
with inputs.nixpkgs.lib;
let
  inherit (inputs.nixpkgs) lib;

  api = makeExtensible (self: {
    lib = import ./lib/default.nix { inherit (inputs.nixpkgs) lib; };

    hixModules = import ./modules/all-modules.nix { inherit inputs; };

    flakeWith = {projectModules ? [], extraModules ? []}:
    import ./lib/build.nix {
      inherit (inputs.nixpkgs) lib;
      inherit (self) hixModules;
      inherit projectModules extraModules;
    };

    flake = projectModules: self.flakeWith { projectModules = toList projectModules; };

    auto = projectModules:
    self.flakeWith { extraModules = [{ auto = true; ifd = false; }]; projectModules = toList projectModules; };

    pro = projectModules:
    self.flakeWith { extraModules = [(import ./modules/pro.nix)]; projectModules = toList projectModules; };

    spec = import ./lib/deps/spec.nix { inherit (self) lib; };
  });

  localOutputs = import ./lib/local.nix { inherit inputs; hix = api; };

in localOutputs // {
  lib = api;
}
