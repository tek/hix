{
  description = "Example";
  inputs.hix.url = "github:tek/hix?ref=0.9.1";
  outputs = {hix, ...}: hix {
    packages.parser = {
      src = ./.;
      library = {
        enable = true;
        dependencies = ["aeson ^>= 2.2" "bytestring"];
      };
      executable.enable = true;
    };
  };
}
