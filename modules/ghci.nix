{
  lib,
  config,
  util,
  ...
}:
let

  inherit (lib) types mkOption;
  inherit (util) outputs;

  cli = config.internal.hixCli.exe;

in {

  options.ghci = {

    setup = mkOption {
      description = ''
      Scripts that should be executed when starting a GHCi command, like imports.
      The attribute name is matched against the command line option `-r` when running apps like `nix run .#ghci`.
      '';
      type = types.attrsOf types.str;
    };

    run = mkOption {
      description = ''
      Test functions for GHCi commands.
      The attribute name is matched against the command line option `-r` when running apps like `nix run .#ghci`.
      '';
      type = types.attrsOf types.str;
    };

    args = mkOption {
      type = types.listOf types.str;
      description = ''
        The command line arguments passed to GHCi.
        Setting this option appends to the defaults, so in order to replace them, use `mkForce`.
        To only override basic GHC options like `-Werror`, use `ghci.ghcOptions`.
      '';
    };

    ghcOptions = mkOption {
      type = types.listOf types.str;
      description = ''
        Command line arguments passed to GHCi that aren't related to more complex Hix config like the preprocessor.

        This option is initialized with values that use the Nix setting `cores` to set the number of
        threads GHCi should use. If you want to control this yourself, use `mkForce` here.
      '';
      default = [];
    };

    preprocessor = mkOption {
      description = ''
        The preprocessor script used to insert extensions and a custom Prelude into source files.
        This is generated by Hix, but may be overridden.
      '';
      type = types.path;
    };

    cores = mkOption {
      description = "The value for the GHC option `-j`, specifying the number of system threads to use.";
      type = types.either types.int types.str;
      default = "";
    };

  };

  options.ghcid = {

    args = lib.mkOption {
      description = ''
      Additional arguments passed to `ghcid`.
      '';
      type = types.listOf types.str;
      default = [];
    };

  };

  config.ghci = {

    setup = {
      hedgehog-property = "import qualified Hedgehog";
      hedgehog-unit = "import qualified Hedgehog";
      tasty-tree = "import qualified Test.Tasty";
    };

    run = {
      hedgehog-property = "Hedgehog.check";
      hedgehog-unit = "Hedgehog.check . Hedgehog.withTests 1 . Hedgehog.property . Hedgehog.test";
      tasty-tree = "Test.Tasty.defaultMain";
    };

    ghcOptions = ["-j${toString config.ghci.cores}" "+RTS" "-A64M" "-RTS"];

    preprocessor = lib.mkDefault (
      util.script "ghci-preprocessor" ''
      ${cli} preproc --context ${outputs.cli-context.json.preproc} --source "$1" --in "$2" --out "$3"
      ''
    );

    args = config.ghci.ghcOptions ++ ["-F" "-pgmF" (toString config.ghci.preprocessor)];
  };
}
