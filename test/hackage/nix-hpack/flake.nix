{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }:
  hix.lib._hix_test {
    packages.root = {
      src = ./.;
      description = "A test package for hix";

      cabal = {

        version = import ./version.nix;
        license = "BSD-2-Clause-Patent";
        license-file = "LICENSE";
        author = "Author McCodeface";

        meta = {
          synopsis = "root";
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
      commitExtraArgs = "--no-gpg-sign";
      tagExtraArgs = "--no-sign";
      tag = true;
      uploadCommand = args: "echo '${builtins.toJSON args}'";
      askVersion = false;
      confirm = false;
      check = false;
      versionFile = "version.nix";
    };
  };
}
