{ lib, config, util, ... }:
with lib;
let

  releaseDrv = import ../lib/release-derivation.nix {
    inherit lib;
    hsLib = config.pkgs.haskell.lib;
  };

  withStatic = pkg: pkg // { static = config.pkgs.haskell.lib.justStaticExecutables pkg; };

  cross = ghc: name: let
    mkCross = cname: cpkgs: withStatic cpkgs.hixPackages.${name};
  in mapAttrs mkCross ghc.pkgs.pkgsCross;

  withCross = env: name: let
    ghc = env.ghc;
  in withStatic ghc.ghc.${name} // { cross = cross ghc name; };

  envDerivations = envs: let
    envPkgs = v: let
      env = config.envs.${v};
    in mapAttrs' (n: d: { name = "${v}-${n}"; value = withCross env n; }) env.derivations;
  in util.foldMapAttrs envPkgs envs;

  devOutputs = let
    env = config.envs.dev;
    ghc = env.ghc.ghc;
    minGhc = config.envs.min.ghc.ghc;
    extra = name: pkg: withCross env name // {
      release = releaseDrv ghc.${name};
      min = minGhc.${name};
    };
    local = mapAttrs extra config.envs.dev.derivations;
  in local // {
    default = local.${config.main};
    min = local.${config.main}.min;
  };

in {
  inherit envDerivations devOutputs;
}
