{
  bounds = {
    local1 = {
      base = {
        lower = "4.15.1.0";
        upper = null;
      };
      containers = {
        lower = "0.7";
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
    lower-local1 = {
      base = "4.15.1.0";
      containers = "0.7";
    };
    lower-local2 = {
      base = "4.15.1.0";
      semigroups = "0.20";
    };
  };
  initial = {
    lower-local1 = {
      containers = "0.7";
    };
    lower-local2 = {
      semigroups = "0.20";
    };
  };
  overrides = {
    lower-local1 = {
      containers = {
        version = "0.7";
        hash = "15i7w8xavx83b0gdiq5a7g3m8k4ghmcy67yhx4b4119x7r4j7w4n";
      };
    };
    lower-local2 = {
      containers = {
        version = "0.7";
        hash = "15i7w8xavx83b0gdiq5a7g3m8k4ghmcy67yhx4b4119x7r4j7w4n";
      };
    };
  };
  resolving = false;
}
