{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = { hix, ... }:
  hix.lib._hix_test {
    packages.root = {
      src = ./.;
      description = "A test package for hix";
      versionFile = "version.nix";

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
      setChangelogVersion = true;
      hooks = {
        postUploadAll = {...}: ''
        print $version > new-version
        '';
        preCommitAll = ''
        print 1 > source
        '';
      };
    };
  };
}
