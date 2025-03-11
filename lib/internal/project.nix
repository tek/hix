{util}: let

  inherit (util) config lib internal;

  noDirs = ''
  The project does not configure any packages. Try adding this to 'flake.nix':
    packages.app.src = ./.;
  Or:
    base = ./.;
  Due to the hermeticity of Nix flakes, the directory cannot be inferred.
  '';

  defaultPackages =
    {
      app = {
        src =
          if config.base == null
          then throw noDirs
          else config.base;
        executable.source-dirs = ".";
      };
    };

  ambiguousMain = ''
  Could not determine the main package.
  This should only happen if all packages depend on each other cyclically.
  If that is not the case, please report a bug at: https://github.com/tek/hix/issues
  You can specify the main package explicitly:
  {
    main = "app";
  }
  '';

  defaultMain = let
      names = config.internal.packageNames;
      count = lib.length names;
    in
      if count == 1
      then lib.head names
      else if count == 0
      then ''
      This action requires at least one package to be defined, as in:
      {
        packages.app = { src = ./.; };
      }
      ''
      else let
        autoMain = internal.packages.selectMain (lib.attrNames config.packages);
      in util.expectJust ambiguousMain autoMain;

  packagePaths = util.mapValues (p: p.src) config.packages;

in {

  inherit
  defaultPackages
  defaultMain
  packagePaths
  ;

}
