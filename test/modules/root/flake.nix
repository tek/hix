{
  description = "hix test project";

  inputs.hix.url = path:HIX;
  inputs.dep1.url = path:BASE/dep1;
  inputs.dep2.url = path:BASE/dep2;

  outputs = { hix, dep1, dep2, ... }:
  hix.lib.flake {
    base = ./.;
    main = "root";
    packages = {
      root = ./.;
      sub = ./sub;
    };
    compat.enable = true;
    overrides = { hackage, source, ... }: {
      stm-chans = hackage "2.0.0" "0afxg1wx0jkkajwcz338hm1ql4rzrj9dkdpkcvdaw04jrzaqwmby";
    };
    depsFull = [dep1 dep2];
    output.amend = project: outputs: {
      stm-chans-version =
        with project.pkgs.lib;
        let
          pred = dep: dep != null && dep.pname == "stm-chans";
          stm-chans = findFirst pred { version = "missing"; } outputs.packages.root.getCabalDeps.libraryHaskellDepends;
        in stm-chans.version;
    };
  };
}
