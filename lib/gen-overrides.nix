{config, lib, util}:
let
  pkgs = config.pkgs;
  overridesFile = config.gen-overrides.file;

  inherit (pkgs) lib;

  pregen = import ./deps/pregen.nix { inherit pkgs; };

  drvAttr = pkg: dump: "  ${pkg} = ${toString dump};";

  genEnv = env: let

    enable = env.enable && env.ghc.gen-overrides;

    attrs = lib.mapAttrsToList drvAttr (pregen.overrides env.ghc.vanillaGhc env.ghc.overrides);

    lines = ["${env.ghc.name} = {"] ++ lib.optionals enable attrs ++ ["};"];

  in lines;

  expr = util.unlines (["{"] ++ lib.concatMap genEnv (lib.attrValues config.envs) ++ ["}"]);

  file = pkgs.writeText "overrides.nix" expr;

  script = pkgs.writeScript "gen-overrides" ''
    #!${pkgs.bashInteractive}/bin/bash
    mkdir -p ${dirOf overridesFile}
    cp ${file} ${overridesFile}
    chmod u+w ${overridesFile}
  '';

in {
  inherit file script;
}
