{ system, inputs }:
let
  pkgs = import inputs.nixpkgs { inherit system; };

  tests = system: import ../test/default.nix { inherit pkgs; };

  docs = import ./doc/default.nix { inherit pkgs inputs; };

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
