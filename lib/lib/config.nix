{lib}: let

  maybeOption = type: args: lib.mkOption ({ type = lib.types.nullOr type; default = null; } // args);

  minGhc = version: env:
  lib.versionAtLeast env.ghc.version version;

  cabalDepPackage = dep:
  if lib.isAttrs dep
  then dep.name
  else dep;

  appDesc = description: program: let
    main =
      if program ? meta.mainProgram
      then "${program}/bin/${program.meta.mainProgram}"
      else "${program}";
  in { type = "app"; program = main; meta = { inherit description; }; };

  app = appDesc "Hix app";

  removeApp = a: removeAttrs a ["program" "type" "meta"];

in {

  inherit
  maybeOption
  minGhc
  cabalDepPackage
  appDesc
  app
  removeApp
  ;

}
