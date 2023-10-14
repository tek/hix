{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = {hix, ...}: hix.lib.flake {
    packages.root = {
      src = ./.;
      versionFile = "root.cabal";
    };
    ifd = true;
    compat.enable = false;
    hackage = {
      commit = true;
      tag = true;
      uploadCommand = args: "echo '${builtins.toJSON args}'";
      askVersion = false;
      confirm = false;
      check = false;
      versionFile = "root.cabal";
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
