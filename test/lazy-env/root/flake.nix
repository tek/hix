{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = {hix, ...}:
  hix.lib._hix_test {
    packages.root.src = ./.;

    envs.lazy = {
      expose = true;
      package-set.compiler = throw "lazy.compiler evaluated";
    };

  };
}
