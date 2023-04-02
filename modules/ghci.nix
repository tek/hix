{
  lib,
  config,
  util,
  ...
}:
with builtins;
with lib;
let

  pkgs = config.pkgs;

  libDir = pkg:
  "$PWD/" + (if pkg == "." then "lib" else "${pkg}/lib");

  srcDir = pkg:
  "$PWD/" + (if pkg == "." then "src" else "${pkg}/src");

  colonSeparated =
    concatStringsSep ":";

  searchPaths = paths:
    "-i${colonSeparated paths}";

  cwdScript = cwd:
  optionalString (cwd != null) ''
    :cd ${cwd}
  '';

  defaultSetup = {
    hedgehog-property = ''
      import Hedgehog (check)
    '';

    hedgehog-unit = ''
      import Hedgehog (check, property, test, withTests)
    '';

    tasty-tree = ''
      import Test.Tasty (defaultMain)
    '';
  };

  defaultRun = {
    hedgehog-property = "check";
    hedgehog-unit = "check . withTests 1 . property . test";
    tasty-tree = "defaultMain";
  };

  builtinTestScripts = {
    hedgehog-property = module: ''
      :load ${module}
      import ${module}
      import Hedgehog (check)
    '';

    hedgehog-unit = module: ''
      :load ${module}
      import ${module}
      import Hedgehog (check, property, test, withTests)
    '';

    tasty-tree = module: ''
      :load ${module}
      import ${module}
      import Test.Tasty (defaultMain)
    '';

    generic = module: ''
      :load ${module}
      import ${module}
    '';
  };

  # TODO these can be specified as functions
  builtinTestRunners = {
    hedgehog-property = name: "check ${name}";
    hedgehog-unit = name: "(check . withTests 1 . property . test) ${name}";
    tasty-tree = name: "defaultMain ${name}";
    generic = id;
  };

  ghciScript = cwd: script:
  let
    scriptText = if isAttrs script then script.script else script;
  in
  cwdScript cwd + scriptText;

  command = {
    packages,
    script,
    search,
    cwd ? null,
  }:
  let
    searchP =
      searchPaths (
        map srcDir (attrValues packages) ++
        map libDir (attrValues packages) ++
        search
      );
    script' = ghciScript cwd script;
    scriptFile = pkgs.writeText "ghci-script" script';
  in {
    inherit searchP scriptFile;
    script = script';
    cmdline = "ghci ${toString config.ghci.args} ${searchP} -ghci-script ${scriptFile}";
  };

  componentConf = c: {
    inherit (c) name;
    inherit (c.env) runner;
    sourceDirs = c.source-dirs;
  };

  packageConf = p: {
    inherit (p) name;
    src = p.subpath;
    components = mapAttrs (_: componentConf) p.componentsSet;
  };

  # TODO add to this set:
  # - runner
  # - search path
  # - imports
  # - component-dependent ghci args
  # - restarts
  # - cwd
  cliJson = pkgs.writeText "hix-cli-json" (toJSON {
    packages = mapAttrs (_: packageConf) config.packages;
    setup = config.ghci.setup;
    run = config.ghci.run;
    args = config.ghci.args;
  });

  cli = config.internal.hixCli.exe;

  flakeAppWithEnv = env: config.pkgs.writeScript "ghci-env-run" ''
  #!${config.pkgs.bashInteractive}/bin/bash
  set -eu
  config=$(cat ${config.ghci.cliJson})
  ghci_cmd=$(${cli} ghci-cmd -c "$config" $@)
  ${env.runner} "eval $ghci_cmd"
  '';

  flakeApp = config.pkgs.writeScript "ghci-run" ''
  #!${config.pkgs.bashInteractive}/bin/bash
  set -eu
  config=$(cat ${config.ghci.cliJson})
  env_runner=$(${cli} component-env -c "$config" $@)
  ghci_cmd=$(${cli} ghci-cmd -c "$config" $@)
  $env_runner "eval $ghci_cmd"
  '';

in {

  options.ghci = with types; {

    setup = mkOption {
      description = "";
      type = attrsOf str;
    };

    run = mkOption {
      description = "";
      type = attrsOf str;
    };

    # runners = mkOption {
    #   type = runnerModule;
    # };

    args = mkOption {
      type = listOf str;
      description = mdDoc ''
        The command line arguments passed to GHCi.
        Setting this option appends to the defaults, so in order to replace them, use `mkForce`.
        To only override basic GHC options like `-Werror`, use `ghci.ghcOptions`.
      '';
    };

    ghcOptions = mkOption {
      type = listOf str;
      description = mdDoc ''
        Command line arguments passed to GHCi that aren't related to more complex Hix config like the preprocessor.

        This option is initialized with values that use the Nix setting `cores` to set the number of
        threads GHCi should use. If you want to control this yourself, use `mkForce` here.
      '';
      default = [];
    };

    scripts = mkOption {
      description = mdDoc ''
        A set of Cabal scripts, to be referenced by entries in `ghcid.commands`.
      '';
      type = attrsOf (functionTo lines);
      example = literalExpression ''
      {
        :set args --port 8080
        :load Main
        import Main
      }
    '';
    };

    runners = mkOption {
      description = mdDoc ''
        A function returning Haskell code that should be executed by GHCi when running a test.
        The argument is the test function name, as passed in via command line arguments.
      '';
      type = attrsOf (functionTo str);
    };

    preprocessor = mkOption {
      description = mdDoc ''
        The preprocessor script used to insert extensions and a custom Prelude into source files.
        This is generated by Hix, but may be overridden.
      '';
      type = path;
    };

    preprocessorExtraCode = mkOption {
      description = mdDoc ''
        Extra Haskell code to be inserted into source files by the preprocessor.
      '';
      type = lines;
      default = "";
    };

    cores = mkOption {
      description = "Cores";
      type = either int str;
      default = ''''${NIX_BUILD_CORES-}'';
    };

    command = mkOption {
      description = mdDoc "Internal API function for creating GHCi commands.";
      type = functionTo unspecified;
    };

    cliJson = mkOption {
      description = "Internal option encoding relevant config as JSON for use with the Hix CLI.";
      type = path;
      readOnly = true;
      default = cliJson;
    };
  };

  config.ghci = {

    setup = defaultSetup;

    run = defaultRun;

    # TODO extract default-language from cabal in the preprocessor
    ghcOptions = ["-j${toString config.ghci.cores}" "+RTS -A64M -RTS"];

    preprocessor = mkDefault (import ../lib/preprocessor.nix {
      inherit pkgs cli;
      extraCode = config.ghci.preprocessorExtraCode;
    });

    args = config.ghci.ghcOptions ++ ["-F" "-pgmF" (toString config.ghci.preprocessor)];

    scripts = builtinTestScripts;

    runners = builtinTestRunners;

    command = mkDefault command;
  };

  config.commands.ghci = {

    command = ''
    config=$(cat ${util.json.ghciFile})
    ghci_cmd=$(${cli} ghci-cmd -c "$config" ''${env_args[@]} ''${cmd_args[@]})
    env_run "eval $ghci_cmd"
    '';

  };
}
