{config, lib, util, ...}:
with lib;
let

  commandModule = import ./command.nix { global = config; inherit util; };

  cli = config.internal.hixCli.exe;

  json = util.json.ghciFile;

in {
  options = with types; {

    hls.genCabal = mkOption {
      description = mdDoc ''
      When running HLS with `nix run .#hls`, the command first generates Cabal files from Hix config to ensure that HLS
      works.
      If that is not desirable, set this option to `false`.
      '';
      type = bool;
      default = true;
    };

    commands = mkOption {
      description = mdDoc ''
      Commands are shell scripts associated with an environment that are exposed as flake apps.
      All commands are accessible as attributes of `.#cmd.<name>`, and those that set `expose = true` are additionally
      exposed at the top level.
      '';
      type = attrsOf (submodule commandModule);
      default = {};
    };

  };

  config.commands = {

    ghci = {
      ghci.enable = true;
      component = true;
      expose = true;
    };

    ghcid = {
      ghci.enable = true;
      ghci.ghcid = true;
      component = true;
      expose = true;
    };

    hls = {
      env = "dev";
      command = ''
      ${if config.hls.genCabal then "nix run .#gen-cabal-quiet" else ""}
      ${config.envs.hls.hls.package}/bin/haskell-language-server $@
      '';
      expose = true;
    };

    run = {
      command = "$@";
    };

  };
}
