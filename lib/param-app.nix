{ config, lib, ... }:
{
  name,
  func,
  params ? [],
  pre ? "",
}:
with lib;
let

  inherit (config) pkgs;

  param = p: "";

  paramLines = concatMapStringsSep "\n" param params;

  script = args: pkgs.writeScriptBin "hix-param-app" ''
  ${func args}
  '';

  shell = args: pkgs.mkShell {
    packages = [(script args)];
  };

  app =
    pkgs.writeScript "${name}-app" ''
      #!${pkgs.zsh}/bin/zsh
      pkg=$1 module=$2 name=$3 type_=$4 runner=''${5-generic}
      ${pre}
      nix develop --impure --expr "
        (builtins.getFlake path:$PWD).legacyPackages.${config.system}.${name} {
          ${paramLines}
        }" -c hix-param-app
  '';

in {
  inherit shell app;
}
