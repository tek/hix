{config, lib, util}:
let
  pkgs = config.pkgs;
  overridesFile = config.gen-overrides.file;

  inherit (pkgs) lib;

  pregen = import ./deps/pregen.nix { inherit config; } { inherit pkgs; };

  drvAttr = pkg: dump: "  ${pkg} = ${toString dump};";

  genEnv = env: let

    enable = env.enable && env.ghc.gen-overrides;

    attrs = lib.mapAttrsToList drvAttr (pregen.overrides env.ghc.vanillaGhc env.ghc.overrides);

    lines = ["${env.ghc.name} = {"] ++ lib.optionals enable attrs ++ ["};"];

  in lines;

  expr = util.unlines (["{"] ++ lib.concatMap genEnv (lib.attrValues config.envs) ++ ["}"]);

  file = pkgs.writeText "overrides.nix" expr;

  script = util.scriptErr "gen-overrides" ''
    if [[ -e "${overridesFile}" ]]
    then
      initial=false
    else
      initial=true
    fi
    mkdir -p ${dirOf overridesFile}
    cp ${file} ${overridesFile}
    chmod u+w ${overridesFile}
    ${lib.optionalString config.gen-overrides.gitAdd ''
      if ${config.pkgs.git}/bin/git status &>/dev/null && [[ $initial == true ]] && [[ -f ${overridesFile} ]]
      then
        ${config.pkgs.git}/bin/git add ${overridesFile}
      fi
    ''}
  '';

in {
  inherit file script;
}
