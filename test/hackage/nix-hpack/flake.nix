{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.flake {
    packages.root = {
      src = ./.;

      cabal = {

        version = import ./version.nix;
        license = "BSD-2-Clause-Patent";

        meta = {
          author = "Author McCodeface";
          synopsis = "root";
          description = "A test package for hix";
          copyright = "2022 Torsten Schmits";
          build-type = "Simple";
          license-file = "LICENSE";
          category = "Test";
        };

      };

      library = {
        enable = true;
        base = "base >= 4 && < 6";
      };

    };
    compat.enable = false;
    hackage = {
      commit = true;
      tag = true;
      uploadCommand = args: "echo '${builtins.toJSON args}'";
      askVersion = false;
      confirm = false;
      check = false;
      versionFile = "version.nix";
    };
  };
}
