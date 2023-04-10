{
  description = "Example";
  inputs.hix.url = "github:tek/hix?ref=0.4.0.2";
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
