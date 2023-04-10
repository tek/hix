{
  description = "Example";
  inputs.hix.url = "github:tek/hix?tag=v0.3.0.0";
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
