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
      versionFile = "root.cabal";
      versionFiles = { root = "root.cabal"; };
    };
  };
}
