{ system, inputs }:
let
  pkgs = import inputs.nixpkgs { inherit system; };
  nmd_pkgs = import inputs.nixpkgs_nmd { inherit system; };

  tests = system: import ../test/default.nix { inherit pkgs; };

  docs = import ./doc/default.nix { inherit inputs; pkgs = nmd_pkgs; };

in {
  packages = {
    docs = docs.docs.html;
    man = docs.docs.manPages;
  };

  apps = {
    test = {
      type = "app";
      program = "${(tests system).main}";
    };
  };
}
