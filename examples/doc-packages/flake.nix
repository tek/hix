{
  description = "Example";
  # TODO
  # inputs.hix.url = "github:tek/hix";
  inputs.hix.url = "path:/home/tek/code/tek/haskell/hix";
  outputs = {hix, ...}: hix.lib.flake {
    packages.parser = {
      src = ./.;
      library = {
        enable = true;
        dependencies = ["aeson ^>= 2.0" "bytestring"];
      };
      executable.enable = true;
    };
  };
}
