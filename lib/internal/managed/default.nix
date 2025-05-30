{util}: let

  conf = util.config.managed;
  minVerbose = conf.verbose || conf.debug;

  state = import ./state.nix { inherit util; };
  env = import ./env.nix { inherit util; };
  cmd = import ./cmd.nix { inherit util; };
  output = import ./output.nix { inherit util; };

in {
  inherit minVerbose state env cmd output;
}
