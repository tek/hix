{
  bounds = {
    root = {
      base = {
        lower = "4.15.1.0";
        upper = null;
      };
      microlens = {
        lower = "0.3.5.1";
        upper = "0.5";
      };
      semigroups = {
        lower = "0.18.4";
        upper = "0.21";
      };
    };
  };
  versions = {
    lower = {
      base = "4.15.1.0";
      microlens = "0.3.5.1";
      semigroups = "0.18.4";
    };
  };
  overrides = {
    lower = {
      microlens = {
        version = "0.3.5.1";
        hash = "sha256-OGQWAtYinJc4A32yVU+3wP9HJZRicveVy0JFQd35Xjc=";
      };
      semigroups = {
        version = "0.18.4";
        hash = "sha256-4L7sKaf9a7dvRhawwwJZ1ZEKHe/owFv8lpZ9gh68p4o=";
      };
    };
  };
  initial = {
    lower = {
      base = "4.15.1.0";
      microlens = "0.4.13.1";
      semigroups = "0.20";
    };
  };
  resolving = false;
}
