{util}: let

  inherit (util) config build;

  showConfig = import ../show-config.nix { inherit util; };

  # TODO use the json method and print in cli
  show-config = util.paramApp {
    name = "show-config";
    func = showConfig;
    params = ["path"];
  };

in {

  show-config.app = show-config.appScript;

  legacyPackages.project =
    util.mergeAll [
      util.project
      {
        inherit (util) build outputs;
        inherit config;
        inherit (build.envs.dev.toolchain) pkgs;
        ghc = build.envs.dev.toolchain.packages;
        ghc0 = build.envs.dev.toolchain.vanilla;
        show-config = show-config.shell;
      }
    ];

}
