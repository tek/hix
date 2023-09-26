{
  description = "Example";
  inputs.hix.url = "github:tek/hix?ref=0.6.7";
  outputs = {hix, ...}: hix {
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
