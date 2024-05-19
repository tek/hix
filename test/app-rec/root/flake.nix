{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }: let

    mod = f: {config, util, ...}: let
      app = name: num: {
        type = "app";
        program = "${util.zscript name "echo '${name} ${toString num}'"}";
      };
    in {
      outputs.apps = f { inherit app; inherit (util) lib; };
    };

  in hix.lib.flake [
    (mod ({lib, app}: {
      a = lib.mkDefault (app "a" 1);
      b = {
        c = app "c" 1;
        d = app "d" 1 // {
          e = app "e" 1;
          f = app "f" 1;
        };
        g.h = app "h" 1;
        g.i = app "i" 1;
      };
    }))
    (mod ({lib, app}: {
      a = app "a" 2;
      b = {
        c = lib.mkForce (app "c" 2);
        d.e = lib.mkForce (app "e" 2);
        d.f = app "f" 2;
        g.h = app "h" 1;
        g.i = app "i" 1;
      };
    }))
  ];
}
