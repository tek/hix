{config, lib, util}:
with lib;
let
  pkgs = config.pkgs;
  overridesFile = config.gen-overrides.file;

  spec = import ./deps/spec.nix { inherit (pkgs) lib; };
  deps = import ./deps/default.nix { inherit pkgs; };

  decl = self: super: pkg: specs: let
    data = spec.reifyPregen { inherit pkgs pkg self super; } specs;
  in { ${pkg} = data; };

  decls = env: let

    ghc = env.ghc.vanillaGhc;

    result = ghc.override {
      overrides = self: super: let
        os = deps.normalize env.ghc.overrides self super;
        decs = concatMapAttrs (decl self super) os;
      in decs // { __hix_pkgs = attrNames os; };
    };

    properPackage = _: a: a != null && !(spec.isOC a);

  in optionals (env.enable && env.ghc.gen-overrides) (filterAttrs properPackage (getAttrs result.__hix_pkgs result));

  drvAttr = pkg: dump: "  ${pkg} = ${toString dump};";

  genEnv = _: env: (util.unlines (["${env.ghc.name} = {"] ++ mapAttrsToList drvAttr (decls env) ++ ["};"]));

  file = pkgs.writeText "overrides.nix" (util.unlines (["{"] ++ mapAttrsToList genEnv config.envs ++ ["}"]));

in pkgs.writeScript "gen-overrides" ''
  #!${pkgs.bashInteractive}/bin/bash
  mkdir -p ${dirOf overridesFile}
  cp ${file} ${overridesFile}
  chmod u+w ${overridesFile}
''
