{
  description = "hix test project";
  # TODO
  # inputs.hix.url = "github:tek/hix";
  inputs.hix.url = "path:/home/tek/code/tek/haskell/hix";
  outputs = {hix, ...}: hix.lib.auto ({config, ...}: {
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
