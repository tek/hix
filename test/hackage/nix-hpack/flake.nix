{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.flake {
    base = ./.;
    packages.root = ./.;
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
    hpack.packages.root = {
      name = "root";
      version = import ./version.nix;
      author = "Author McCodeface";
      synopsis = "root";
      description = "A test package for hix";
      copyright = "2022 Torsten Schmits";
      build-type = "Simple";
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      category = "Test";
      library = {
        source-dirs = "lib";
        dependencies = ["base >= 4 && < 6"];
      };
    };
  };
}
