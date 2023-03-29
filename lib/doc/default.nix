{ pkgs, config, }:
with pkgs.lib;
let
  lib = pkgs.lib;
  global = config;

  options = import ./options.nix { inherit pkgs; };
  inherit (options) namePh;

  util = import ../with-config.nix { inherit config lib util; };

  mod-ghc = options.module "ghc" { inherit global util; };

  mod-cabal-options = options.module "cabal-options" { inherit global util; };

  compExcept = [["source-dirs"] ["name"]];
  compMultiExcept = [[namePh "source-dirs"] [namePh "name"]];

  packageExclude = [
    { type = "sub"; path = ["cabal-config"]; }
    { type = "sub"; path = ["library"]; except = compExcept; }
    { type = "sub"; path = ["executable"]; except = compExcept; }
    { type = "sub"; path = ["executables"]; except = compMultiExcept; }
    { type = "sub"; path = ["test"]; except = compExcept; }
    { type = "sub"; path = ["tests"]; except = compMultiExcept; }
    { type = "sub"; path = ["benchmark"]; except = compExcept; }
    { type = "sub"; path = ["benchmarks"]; except = compMultiExcept; }
  ];

  mod-package = options.moduleWithout packageExclude "package" { global = config; inherit util; };

  text = content: { type = "text"; inherit content; };
  opt = name: options: { type = "options"; content = { inherit name options; }; };

  chapters = [
    {
      tag = "intro";
      heading = "Introduction";
      fragments = [
        (text about)
      ];
    }
    {
      tag = "hix-build";
      heading = "Declaring Hix builds";
      fragments = [
        (text packages)
        (opt "cabal" mod-cabal-options)
        (text packageOptions)
        (opt "package" mod-package)
      ];
    }
    {
      tag = "devshell";
      heading = "Development shells";
      fragments = [
        (text devshell)
      ];
    }
  ];

  # TODO remove revision
  man = import ./manual.nix { inherit pkgs util header chapters; revision = "default"; };

in {
  json = man.optionsJSON;
  pkg = man.renderManual;
}
