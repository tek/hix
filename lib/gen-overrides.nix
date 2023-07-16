{config, lib, util}:
with lib;
let
  pkgs = config.pkgs;
  overridesFile = config.gen-overrides.file;

  spec = import ./deps/spec.nix { inherit (pkgs) lib; };
  deps = import ./deps/default.nix { inherit pkgs; };

  # TODO does it matter that self and super are the same?
  decl = ghc: pkg: specs: let
    data = spec.reifyPregen { inherit pkgs pkg; self = ghc; super = ghc; } specs;
  in optionalAttrs (data != null) { ${pkg} = data; };

  decls = env: let
    ghc = env.ghc.vanillaGhc;
  in optionals (env.enable && env.ghc.gen-overrides) (concatMapAttrs (decl ghc) (deps.normalize env.ghc.overrides ghc ghc));

  drvAttr = pkg: dump: "  ${pkg} = ${toString dump};";

  genEnv = _: env: (util.unlines (["${env.ghc.name} = {"] ++ mapAttrsToList drvAttr (decls env) ++ ["};"]));

  file = pkgs.writeText "overrides.nix" (util.unlines (["{"] ++ mapAttrsToList genEnv config.envs ++ ["}"]));

in pkgs.writeScript "gen-overrides" ''
  #!${pkgs.bashInteractive}/bin/bash
  mkdir -p ${dirOf overridesFile}
  cp ${file} ${overridesFile}
  chmod u+w ${overridesFile}
''
