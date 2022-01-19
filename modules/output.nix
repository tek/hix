{ lib, config, ... }:
with lib;
with types;
let
in {
  options.output = {
    systems = mkOption {
      type = listOf str;
      description = "The systems for which to create outputs.";
      default = ["x86_64-linux"];
    };

    overrideMain = mkOption {
      type = unspecified;
      default = id;
      description = ''
        A function that allows customization of the generated dev package set.
      '';
    };

    transform = mkOption {
      type = functionTo (functionTo unspecified);
      default = _: id;
      description = ''
        A function taking the dev project and the generated outputs and returning modified outputs.
        The return value is not merged with the original outputs.
      '';
    };

    amend = mkOption {
      type = functionTo (functionTo unspecified);
      default = _: id;
      description = ''
        A function taking the dev project and the generated outputs and returning additional outputs.
        The return value is merged with the original outputs.
      '';
    };
  };
}
