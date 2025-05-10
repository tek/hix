{
  inputs = { nixpkgs.url = "github:nixos/nixpkgs/b139e44d78c36c69bcbb825b20dbfa51e7738347"; };
  outputs = {self, nixpkgs}: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
    drv = pkgs.writeShellScriptBin "greet" "echo Hello";
  in {
    apps.x86_64-linux.greet = {
      type = "app";
      program = "${drv}/bin/greet";
    };
  };
}
