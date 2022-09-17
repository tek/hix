{
  description = "hix test project";

  inputs.hix.url = path:HIX;
  inputs.dep1.url = path:BASE/dep1;
  inputs.dep2.url = path:BASE/dep2;

  outputs = { hix, dep1, dep2, ... }:
  hix.lib.flake ({ lib, ... }: {
    base = ./.;
    main = "root";
    packages = {
      root = ./.;
      sub = ./sub;
    };
    hpack.packages.root = {
      name = "root";
      version = "1";
      library = {
        source-dirs = "src";
        dependencies = [
          "base >= 4 && < 6"
          "sub"
          "dep2"
          "dep1"
          "stm-chans"
        ];
      };
      executables.run = {
        main = "Main.hs";
        source-dirs = "app";
        dependencies = [
          "base >= 4 && < 6"
          "root"
        ];
      };
    };
    compat.enable = true;
    overrides = { hackage, source, ... }: {
      stm-chans = hackage "2.0.0" "0afxg1wx0jkkajwcz338hm1ql4rzrj9dkdpkcvdaw04jrzaqwmby";
    };
    depsFull = [dep1 dep2];
    output.amend = project: outputs: {
      stm-chans-version =
        with lib;
        let
          pred = dep: dep != null && dep.pname == "stm-chans";
          stm-chans = findFirst pred { version = "missing"; } outputs.packages.root.getCabalDeps.libraryHaskellDepends;
        in stm-chans.version;
      };
    });
}
