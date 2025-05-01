{util}:
let
  inherit (util) build lib config;

  pkgs = config.pkgs;
  overridesFile = config.gen-overrides.file;

  drvAttr = pkg: dump: "  ${pkg} = ${toString dump};";

  genEnv = env: let
    toolchain = build.envs.${env.name}.toolchain;

    pregen = import ./deps/pregen.nix { inherit config; } { pkgs = toolchain.pkgs; };

    enable = env.enable && env.package-set.gen-overrides;

    attrs = lib.mapAttrsToList drvAttr (pregen.overrides toolchain.vanilla toolchain.overrides);

    lines = ["${env.name} = {"] ++ lib.optionals enable attrs ++ ["};"];

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
      if ${pkgs.git}/bin/git status &>/dev/null && [[ $initial == true ]] && [[ -f ${overridesFile} ]]
      then
        ${pkgs.git}/bin/git add ${overridesFile}
      fi
    ''}
  '';

in {
  inherit file script;
}
