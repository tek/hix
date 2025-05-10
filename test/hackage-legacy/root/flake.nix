{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = {hix, ...}: hix.lib._hix_test {
    packages.root = {
      src = ./.;
      versionFile = "root.cabal";
    };
    ifd = true;
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
