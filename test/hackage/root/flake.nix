{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.flake {
    packages.root.src = ./.;
    packages.root.versionFile = "root.cabal";
    compat.enable = false;
    hackage = {
      commit = true;
      tag = true;
      uploadCommand = args: "echo '${builtins.toJSON args}'";
      askVersion = false;
      confirm = false;
      check = false;
      versionFile = "root.cabal";
    };
  };
}
