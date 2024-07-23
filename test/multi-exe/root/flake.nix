{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }:
  hix.lib._hix_test {
    main = "pkg1";

    packages = {

      pkg1 = {
        src = ./pkg1;

        executable = {
          enable = true;
          name = "pkg1_main";
        };

        executables = {
          exe1 = { name = "pkg1_exe1"; };
          exe2 = {};
          exe3 = {};
        };

      };

      pkg2 = {
        src = ./pkg2;

        executable.enable = true;

        executables = {
          exe1 = {};
          exe2 = { name = "pkg2_exe2"; };
          exe3 = {};
        };

      };

      pkg3 = {
        src = ./pkg3;

        expose = {
          packages = false;
          apps = false;
          checks = false;
          scoped = true;
        };

        executable.enable = true;

        executables = {
          exe1 = {};
          exe2 = {};
          exe3 = {};
        };

      };

      pkg4 = {
        src = ./pkg4;

        executable.enable = true;

        executables = {
          exe1 = {};
          exe2 = {};
          exe3 = {};
        };

      };

    };

    envs.dev.packages = ["pkg1" "pkg2" "pkg3"];
    envs.p2 = {
      expose = { apps = true; };
      packages = ["pkg2"];
    };
  };
}
