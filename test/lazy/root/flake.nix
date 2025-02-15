{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }: hix.lib._hix_test {
    ifd = true;

    packages = {

      one = {
        src = ./one;
        executable.source-dirs = ".";
      };

      two = {
        src = ./two;
        executable.source-dirs = ".";
        cabal.dependencies = ["nonexistent-package"];
      };

    };

    outputs.packages.three = throw "arggghhh";

  };
}
