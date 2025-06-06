{
  description = "hix test project";
  inputs.hix.url = "HIX";
  outputs = {hix, ...}: hix.lib._hix_test {
    envs = {
      one.env = { number = 1; };
      two.env = { number = 2; };
      three.env = { number = 3; };
      four.env = { number = 4; };
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
