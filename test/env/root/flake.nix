{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }: hix.lib.flake ({config, ...}: {

    envs = {
      one.env = { number = 1; };
      two.env = { number = 2; };
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

  });
}
