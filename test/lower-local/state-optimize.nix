{
  bounds = {
    local1 = {
      base = {
        lower = "4.15.1.0";
        upper = null;
      };
      containers = {
        lower = "0.5.7.0";
        upper = null;
      };
    };
    local2 = {
      base = {
        lower = "4.15.1.0";
        upper = null;
      };
      semigroups = {
        lower = "0.18.1";
        upper = null;
      };
    };
  };
  versions = {
    lower-local1 = {
      base = "4.15.1.0";
      containers = "0.5.7.0";
    };
    lower-local2 = {
      base = "4.15.1.0";
      semigroups = "0.18.1";
    };
  };
  overrides = {
    lower-local1 = {
      containers = {
        version = "0.5.7.0";
        hash = "0hwknfbrv1yimsf79n309gnj093cac877s0w2rnp039z61lln4j4";
      };
    };
    lower-local2 = {
      semigroups = {
        version = "0.18.1";
        hash = "1z714pdlx2klcsp0h9qrsm5if54fd07zy9rwgqr62gp0akvr4w78";
      };
    };
  };
  initial = {
    lower-local1 = {
      base = "4.15.1.0";
      containers = "0.7";
    };
    lower-local2 = {
      base = "4.15.1.0";
      semigroups = "0.20";
    };
  };
  resolving = false;
}
