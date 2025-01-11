{util}: let

  inherit (util) config;

  showConfig = import ../show-config.nix { inherit util; };

  # TODO use the json method and print in cli
  show-config = util.paramApp {
    name = "show-config";
    func = showConfig;
    params = ["path"];
  };

in {

  show-config.app = show-config.appScript;

  legacyPackages = {
    inherit config;
    inherit (config.envs.dev.ghc) pkgs ghc;
    ghc0 = config.envs.dev.ghc.vanillaGhc;
    show-config = show-config.shell;
  };

}
