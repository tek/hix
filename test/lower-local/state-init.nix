{
  bounds = {
    local1 = {
      base = {
        lower = "4.15.1.0";
        upper = null;
      };
      containers = {
        lower = "0.8";
        upper = null;
      };
    };
    local2 = {
      base = {
        lower = "4.15.1.0";
        upper = null;
      };
      semigroups = {
        lower = "0.20";
        upper = null;
      };
    };
  };
  versions = {
    latest-local1 = {};
    latest-local2 = {};
    lower-local1 = {
      base = "4.15.1.0";
      containers = "0.8";
    };
    lower-local2 = {
      base = "4.15.1.0";
      semigroups = "0.20";
    };
  };
  initial = {
    latest-local1 = {};
    latest-local2 = {};
    lower-local1 = {
      containers = "0.8";
    };
    lower-local2 = {
      semigroups = "0.20";
    };
  };
  overrides = {
    lower-local1 = {
      containers = {
        version = "0.8";
        hash = "06mmyljfj41hg5rzr9d2fb61gd2a11waicpk7dcy3hxrqvfgs8yc";
      };
    };
    lower-local2 = {
      containers = {
        version = "0.8";
        hash = "06mmyljfj41hg5rzr9d2fb61gd2a11waicpk7dcy3hxrqvfgs8yc";
      };
    };
  };
  resolving = false;
}
