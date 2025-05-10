{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = {hix, ...}: hix.lib._hix_test {
    packages.containers = {
      src = ./.;
      versionFile = "containers.cabal";
    };
    ifd = true;
    compat.enable = false;
    hackage = {
      commit = true;
      tag = true;
      askVersion = false;
      confirm = false;
      check = false;
      versionFile = "containers.cabal";
      setChangelogVersion = true;
      cabalArgs = "--config-file=$CABAL_CONFIG";
    };
  };
}
