inputs:
with builtins;
with inputs.nixpkgs.lib;
let
  inherit (inputs.nixpkgs) lib;

  localOutputs =
    inputs.flake-utils.lib.eachSystem ["x86_64-linux"] (system: import ./lib/local.nix { inherit system inputs; });

  compat = import ./lib/compat.nix { inherit lib; };

  api = rec {
    modules = projectModules: import ./modules/all-modules.nix {
      inherit inputs;
      projectModules = toList projectModules;
    };
    flake = projectModules: import ./modules/build.nix { inherit lib; modules = modules projectModules; };
    obeliskOverrides = import ./obelisk/overrides.nix { inherit (inputs) obelisk; };
  };

in localOutputs // {
  lib = api;

  flake = args:
  let msg = "The function 'flake' is deprecated. Please use 'lib.flake'.";
  in warn msg (api.flake (compat.check args));

  obeliskOverrides = compat.warn "obeliskOverrides" "lib.obeliskOverrides" api.obeliskOverrides;
}
