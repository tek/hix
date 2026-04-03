{util}:
let
  inherit (util) build lib config;

  pkgs = config.pkgs;
  overridesFile = config.gen-overrides.file;

  pretty = lib.generators.toPretty { allowPrettyValues = true; };

  literal = expr: { __pretty = _: toString expr; val = null; };

  genEnv = env: let
    toolchain = build.envs.${env.name}.toolchain;

    pregen = import ./deps/pregen.nix { inherit config; } { pkgs = toolchain.pkgs; };

    name = env.package-set.name;

    enable = env.enable && env.package-set.gen-overrides;

    value = lib.mapAttrs (_: literal) (
      lib.optionalAttrs enable (pregen.overrides toolchain.vanilla toolchain.overrides)
    );
  in {
    inherit name value;
  };

  envResults = map genEnv (lib.attrValues config.envs);

  uniqueByName = lib.filterAttrs (_: v: v != {}) (lib.listToAttrs envResults);

  expr = pretty uniqueByName;

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

