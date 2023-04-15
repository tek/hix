{
  description = "hix test project";

  inputs.hix.url = path:HIX;
  inputs.dep1.url = path:BASE/dep1;
  inputs.dep2.url = path:BASE/dep2;

  outputs = { hix, dep1, dep2, ... }: hix.lib.flake ({ config, lib, ... }: {
    main = "root";
    packages = {
      root = {
        src = ./.;
        cabal = {
          version = "1";
          base = "base >= 4 && < 6";
        };
        library = {
          enable = true;
          source-dirs = "src";
          dependencies = [
            config.packages.sub.dep.exact
            "dep2"
            "dep1"
            "stm-chans"
          ];
        };
        executables.run = { source-dirs = "app"; };
        override = {minimal, ...}: minimal;
      };
      sub = {
        src = ./sub;
        library.enable = true;
        library.source-dirs = "src";
      };
    };
    compat.enable = true;
    overrides = { hackage, source, ... }: {
      stm-chans = hackage "2.0.0" "0afxg1wx0jkkajwcz338hm1ql4rzrj9dkdpkcvdaw04jrzaqwmby";
    };
    depsFull = [dep1 dep2];
    output.final = config.outputs // {
      stm-chans-version =
        with lib;
        let
          pred = dep: dep != null && dep.pname == "stm-chans";
          stm-chans = findFirst pred { version = "missing"; } config.outputs.packages.root.getCabalDeps.libraryHaskellDepends;
        in stm-chans.version;
      };
    });
}
