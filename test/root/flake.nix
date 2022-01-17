{
  description = "hix test project";

  inputs.hix.url = path:HIX;
  inputs.dep1.url = path:BASE/dep1;
  inputs.dep2.url = path:BASE/dep2;

  outputs = { hix, dep1, dep2, ... }:
  hix.flake {
    base = ./.;
    main = "root";
    packages = {
      root = ./.;
      sub = ./sub;
    };
    compat = false;
    overrides = { hackage, source, ... }: {
      dep1 = source.root dep1;
      dep2 = source.root dep2;
      stm-chans = hackage "2.0.0" "0afxg1wx0jkkajwcz338hm1ql4rzrj9dkdpkcvdaw04jrzaqwmby";
    };
    deps = [dep1 dep2];
    modify = project: outputs: {
      stm-chans-version =
        with project.pkgs.lib;
        let
          pred = dep: dep != null && dep.pname == "stm-chans";
          stm-chans = findFirst pred { version = "missing"; } outputs.packages.root.getCabalDeps.libraryHaskellDepends;
        in stm-chans.version;
    };
  };
}
