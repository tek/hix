{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }: {
    ghcid =
      let

        flake = hix.lib.flake {
          base = ./.;
          packages.root = ./.;
          ghci.prelude = true;
          ghcid.commands = {
            test = {
              script = ''
              :load Root.Lib
              import Root.Lib
              '';
              test = ''putStrLn "success"'';

              shellConfig.vm.enable = true;
            };
          };
          ghcid.testConfig = { type, ... }: {
            search = if type == "integration" then ["extra-search"] else [];
          };
        };

        ghcid =
          flake.legacyPackages.x86_64-linux.config.ghcid;

      in {
        inherit (ghcid.shells.test.ghciCommand) script;

        testConfig_searchPath = (ghcid.run.override {
          type = "intergration";
        }).ghciCommand.searchP;

        inherit (ghcid.shells.test) mainScript;
      };
  };
}
