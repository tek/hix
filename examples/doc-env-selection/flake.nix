{
  description = "hix test project";
  inputs.hix.url = "github:tek/hix?ref=0.8.0";
  outputs = {hix, ...}: hix ({config, ...}: {
    envs = {
      one.env = { number = 1; };
      two.env = { number = 2; };
      three.env = { number = 3; };
    };

    packages.root = {
      src = ./.;
      executable.env = config.envs.two;
    };

    commands.number = {
      env = config.envs.one;
      command = ''
      echo $number
      '';
      component = true;
    };

  });
}
