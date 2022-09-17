{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  let
    flake = hix.lib.flake {
      base = ./.;
      packages.root = ./.;
      overrides = { hackage, ... }: {
        incipit-base = hackage "0.1.0.0" "0pw3wr3yjwg4zphndnzazb7ycmjmrfqn57sjlkiqlb4hnwxk1xmk";
      };
      ghci.preludePackage = "incipit-base";
      ghci.preludeModule = "Incipit.Prelude";
      ghcid.commands = {
        test = {
          script = ''
          :load Root.Lib
          import Root.Lib
          putStrLn string
          '';
          test = ''putStrLn "success"'';
          shellConfig.vm.enable = true;
        };
      };
      ghcid.testConfig = { type, ... }: {
        search = if type == "integration" then ["extra-search"] else [];
      };
    };

    cfg = flake.legacyPackages.x86_64-linux.config;
    pkgs = cfg.internal.basicPkgs;
    inherit (pkgs.lib) splitString concatStringsSep take;

    ghci =
      cfg.ghci;

    ghcid =
      cfg.ghcid;

    cmdArgs = {
      packages.root = ./.;
      script = ''
        :load Root.Lib
        import Root.Lib
        putStrLn "before"
        putStrLn string
        putStrLn "after"
      '';
      search = [];
    };

    cmd = cfg.internal.basicPkgs.writeScript "ghci-test" ''
      nix develop -c ${ghcid.shells.test.ghciCommand.cmdline}
    '';

  in flake // {
    apps.x86_64-linux.ghci-test = {
      type = "app";
      program = "${cmd}";
    };
    ghcid = {
      inherit (ghcid.shells.test.ghciCommand) script;
      inherit (ghcid.shells.test.ghciCommand) cmdline;

      testConfig_searchPath =
        let
          path = (ghcid.run { type = "integration"; }).ghciCommand.searchP;
        in concatStringsSep ":" (take 4 (splitString ":" path));

      inherit (ghcid.shells.test) mainScript;
    };
  };
}
