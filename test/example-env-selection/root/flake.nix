{
  description = "hix test project";
  inputs.hix.url = "github:tek/hix?ref=0.9.1";
  outputs = {hix, ...}: hix {
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
      echo $(( $number + ''${1-0} ))
      '';
      expose = true;
      component = true;
    };

  };
}
