{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = {hix, ...}: hix.lib._hix_test ({config, ...}: {

    envs = {
      one.env = { number = 1; };
      two.env = { number = 2; };
      two.buildInputs = pkgs: [pkgs.socat];
      three.env = { number = 3; };
    };

    packages.root = {
      src = ./.;
      executable.env = "two";
    };

    commands.number = {
      env = "one";
      command = ''
      echo $number
      '';
      component = true;
    };

    commands.number-nocomp = {
      env = "one";
      command = ''
      echo $number
      '';
    };

  });
}
