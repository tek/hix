{testlib, pkgs, ...}: let

  hookPackage = pkgs.writeScriptBin "release-hook-marker" ''
  #!/usr/bin/env bash
  echo "hook-executed" > hook-result
  '';

  main = import ./main.nix { inherit pkgs; };

  withHackage = testlib.hackage.withServer { cabalConf = false; } main;

in withHackage // {
  genCabal = true;
  git = true;
  path = withHackage.path ++ [hookPackage];
}
