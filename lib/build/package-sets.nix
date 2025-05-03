{util}: let

  inherit (util) internal lib;

  customize = conf: pkgs: default:
  if conf.customize == null
  then default
  else conf.customize (default // { inherit pkgs; })
  ;

  create = conf: let

    compiler = internal.modules.resolveExtensibleModule "compilers" conf.compiler;

  in compiler.extend (final: prev: {

    inherit conf;
    inherit (conf) overrides;

    overlay = pkgsFinal: pkgsPrev: let

      overlayPrev = prev.overlay pkgsFinal pkgsPrev;

      hixMetadata = {
        hix-nixpkgs-rev = final.pkgs.rev;
        hix-name = conf.name;
      };

      packages = overlayPrev.haskell.packages.${final.tag}.override {
        overrides = internal.overrides.forPackageSet {
          pkgs = pkgsPrev;
          packageSet = conf;
        };
      } // hixMetadata;

      custom = customize conf pkgsFinal packages;

    in lib.updateManyAttrsByPath [{ path = ["haskell" "packages" final.tag]; update = _: custom; }] overlayPrev;

    pkgs =
      if conf.cross == null
      then prev.pkgs
      else prev.pkgs.pkgsCross.${conf.cross}
      ;

  });

in util.mapValues create util.config.package-sets // { __functor = _: create; }
