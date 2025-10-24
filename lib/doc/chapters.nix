{config, pkgs, util, prose}: let

  inherit (util) internal;

  global = config;
  lib = pkgs.lib;

  options = import ./options.nix { inherit pkgs; };
  inherit (options) namePh;

  cabalOptionsModules = [
    (options.importMod "cabal-options" { inherit global util; })
    (options.importMod "cabal-component" { inherit global util; } {
      pkgName = "<package>";
      src = "<src>";
      sort = "<component type>";
      desc = "<component type>";
      suffixOption = null;
      single = false;
    })
  ];

  mod-cabal-options = options.modulesWithout [] cabalOptionsModules;

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
    { type = "full"; path = ["internal"]; }
  ];
  mod-package = options.moduleWithout packageExclude "package" { global = config; inherit util; };

  mod-packageExpose = options.modules [
    (options.importMod "expose" { inherit util; type = "package"; default = true; })
    (options.importMod "package-expose" { inherit util; })
  ];

  envExclude = [
    { type = "sub"; path = ["ghc"]; }
    { type = "sub"; path = ["services"]; }
    { type = "full"; path = ["internal"]; }
  ];
  mod-env = options.moduleWithout envExclude "env" { global = config; inherit util; };

  mod-envExpose = options.modules [
    (options.importMod "expose" { inherit util; type = "env"; default = false; })
    (options.importMod "env/expose" { inherit util; })
  ];

  mod-nixpkgs = options.module "nixpkgs" { inherit util; };

  mod-compiler = options.module "compiler" { inherit util; };

  mod-ghc-build = options.module "ghc-build" { inherit util; };

  mod-package-set = options.module "package-set" { inherit util; };

  mod-toolchain = options.module "toolchain" { inherit util; };

  commandExclude = [
    { type = "sub"; path = ["env"]; }
  ];
  mod-command = options.moduleWithout commandExclude "command" { global = config; inherit util; };

  ghciExclude = [
  ];
  mod-ghci = options.moduleWithout ghciExclude "ghci" { inherit config lib util; };

  serviceExclude = [
  ];
  mod-service = options.moduleWithout serviceExclude "service" { inherit util; };

  hackageExclude = [
    { type = "sub"; path = ["output"]; }
  ];
  mod-hackage = options.moduleWithout hackageExclude "hackage" { inherit config lib util; };

  mod-managed = options.moduleWithout [] "managed" { inherit config lib util; };

  mod-ui = options.module "ui" { inherit config lib util; inherit (util) outputs; };

  generalModules = [
    (options.importMod "systems" { inherit config lib; })
    (options.importMod "system" { inherit config lib; })
    (options.importMod "input" { inherit config lib; })
    (options.importMod "basic" { inherit config lib util internal; })
    (options.importMod "envs" { inherit config lib util; })
    (options.importMod "services" { inherit config lib util; })
    (options.importMod "commands" { inherit config lib util; })
    (options.importMod "overrides" { inherit config lib util; })
    (options.importMod "output" { inherit config lib util; inherit (util) outputs; })
    (options.importMod "build-tools" { inherit config lib util; })
  ];
  generalExclude = [
    { type = "sub"; path = ["envs"]; }
    { type = "sub"; path = ["services"]; }
    { type = "sub"; path = ["commands"]; }
    { type = "sub"; path = ["devGhc"]; }
    { type = "sub"; path = ["packages"]; }
    { type = "sub"; path = ["cabal-config"]; }
    { type = "full"; path = ["internal"]; }
  ];
  mod-general = options.modulesWithout generalExclude generalModules;

  text = content: { type = "text"; inherit content; };
  optWith = extra: name: header: options: { type = "options"; content = { inherit name options header extra; }; };
  opt = optWith "";

  chapters = [
    {
      tag = "intro";
      heading = "Introduction";
      fragments = [
        (text prose.about)
      ];
    }
    {
      tag = "hix-build";
      heading = "Declaring Hix builds";
      fragments = [
        (text prose.packages)
        (optWith prose.cabalOptionsHeader "cabal" "Cabal" mod-cabal-options)
        (text prose.package)
        (opt "package" "Package" mod-package)
        (opt "package-expose" "Package exposure" mod-packageExpose)
        (opt "general" "General" mod-general)
        (text prose.ifd)
        (text prose.ui)
        (opt "ui" "UI" mod-ui)
      ];
    }
    {
      tag = "env-cmd";
      heading = "Environments and commands";
      fragments = [
        (text prose.environments)
        (opt "env" "Environment" mod-env)
        (opt "env-expose" "Environment exposure" mod-envExpose)
        (opt "nixpkgs" "Nixpkgs" mod-nixpkgs)
        (opt "compiler" "Compiler" mod-compiler)
        (opt "ghc-build" "GHC build" mod-ghc-build)
        (opt "package-set" "Package set" mod-package-set)
        (opt "toolchain" "Toolchain" mod-toolchain)
        (opt "command" "Command" mod-command)
        (opt "ghci" "GHCi(d)" mod-ghci)
        (opt "service" "Service" mod-service)
      ];
    }
    {
      tag = "tools";
      heading = "Other tools";
      fragments = [
        (text prose.hls)
        (text prose.compat)
        (text prose.upload)
        (opt "hackage" "Hackage" mod-hackage)
        (text prose.tags)
        (text prose.cross)
        (text prose.managed)
        (opt "managed" "Managed dependencies" mod-managed)
        (text prose.misc)
      ];
    }
  ];

in chapters
