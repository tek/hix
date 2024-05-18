{lib, ...}:
with lib;
{
  options = with types; {
    inputs = mkOption {
      description = "The inputs of the Hix flake.";
      type = lazyAttrsOf unspecified;
      readOnly = true;
    };
  };
}
