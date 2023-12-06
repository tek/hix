{
  bounds = {
    root = {
      microlens = ">=0.3.5.1 && < 0.5";
      semigroups = ">=0.18.4 && <0.21";
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
  lowerInit = {};
  resolving = false;
}
