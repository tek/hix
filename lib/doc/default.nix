{ inputs, pkgs, }:
with pkgs.lib;
let
  lib = pkgs.lib;

  modules = [{ inherit (pkgs) system; }] ++ import ../../modules/all-modules.nix { inherit inputs; projectModules = []; };
  config = (evalModules { inherit modules; }).config;

  global = config;

  options = import ./options.nix { inherit pkgs; };
  inherit (options) namePh;

  util = import ../with-config.nix { inherit config lib util; };

  prose = import ./prose.nix {};

  mod-ghc = options.module "ghc" { inherit global util; };

  # cabalOptionsExclude = [
  #   { type = "eq"; path = ["language"]; }
  # ];

  # TODO remove if it works
  # mod-cabal-options = options.moduleWithout cabalOptionsExclude "cabal-options" { inherit global util; };
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
    { type = "eq"; path = ["components"]; }
    { type = "eq"; path = ["componentsSet"]; }
  ];

  mod-package = options.moduleWithout packageExclude "package" { global = config; inherit util; };

  envExclude = [
    { type = "sub"; path = ["ghc"]; }
    # { type = "sub"; path = ["vm"]; }
    { type = "sub"; path = ["services"]; }
  ];

  mod-env = options.moduleWithout envExclude "env" { global = config; inherit util; };

  commandExclude = [
    { type = "sub"; path = ["env"]; }
  ];

  mod-command = options.moduleWithout commandExclude "command" { global = config; inherit util; };

  text = content: { type = "text"; inherit content; };
  opt = name: options: { type = "options"; content = { inherit name options; }; };

  chapters = with prose; [
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
        (text environments)
        (opt "env" mod-env)
        (text commandOptionsHeader)
        (opt "command" mod-command)
      ];
    }
    # {
    #   tag = "devshell";
    #   heading = "Development shells";
    #   fragments = [
    #     (text devshell)
    #   ];
    # }
  ];

  render = import ./render.nix { inherit pkgs util chapters; inherit (prose) header; };

in {
  json = render.optionsJSON;
  html = render.renderManual;
}
