{config, lib, util, ...}:
with lib;
let

  commandModule = import ./command.nix { global = config; inherit util; };

in {
  options = with types; {

    hls.genCabal = mkOption {
      description = ''
      When running HLS with `nix run .#hls`, the command first generates Cabal files from Hix config to ensure that HLS
      works.
      If that is not desirable, set this option to `false`.
      '';
      type = bool;
      default = true;
    };

    commands = mkOption {
      description = ''
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

    # TODO The executable could probably be moved to `hix-build-tools`.
    ghcid = {
      ghci.enable = true;
      ghci.ghcid = true;
      component = true;
      expose = true;
    };

    hls = {
      command = ''
      ${if config.hls.genCabal then "nix run .#gen-cabal-quiet" else ""}
      ${config.envs.hls.hls.package}/bin/haskell-language-server "$@"
      '';
      expose = true;
    };

    run = {
      command = "$@";
    };

  };
}
