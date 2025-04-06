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
      local1 = {
        lower = "0.1.0.0";
        upper = null;
      };
      semigroups = {
        lower = "0.18.1";
        upper = null;
      };
    };
  };
  versions = {
    latest-local1 = {};
    latest-local2 = {};
    lower-local1 = {
      base = "4.15.1.0";
      containers = "0.5.7.0";
    };
    lower-local2 = {
      base = "4.15.1.0";
      local1 = "0.1.0.0";
      semigroups = "0.18.1";
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
        version = "0.5.7.0";
        hash = "0hwknfbrv1yimsf79n309gnj093cac877s0w2rnp039z61lln4j4";
        repo = "hackage.haskell.org";
      };
    };
    lower-local2 = {
      local1 = {
        local = true;
      };
      semigroups = {
        version = "0.18.1";
        hash = "1z714pdlx2klcsp0h9qrsm5if54fd07zy9rwgqr62gp0akvr4w78";
        repo = "hackage.haskell.org";
      };
    };
  };
  solver = {
    lower-local1 = {};
    lower-local2 = {};
  };
  resolving = false;
}
