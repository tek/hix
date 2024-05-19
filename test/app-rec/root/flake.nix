{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }: let

    mod = f: {config, util, ...}: let
      app = name: num: {
        type = "app";
        program = "${config.pkgs.writeScript name "echo '${name} ${toString num}'"}";
      };
    in {
      outputs.apps = f { inherit app; inherit (util) lib; };
    };

  in hix.lib.flake [
    (mod ({lib, app}: {
      a = lib.mkDefault (app "a" 1);
      sub = {
        b = app "b" 1;
        c = app "c" 1 // {
          d = app "d" 1;
          e = app "e" 1;
        };
      };
    }))
    (mod ({lib, app}: {
      a = app "a" 2;
      sub = {
        b = lib.mkForce (app "b" 2);
        c.d = lib.mkForce (app "d" 2);
        c.e = app "e" 2;
      };
    }))
  ];
}
