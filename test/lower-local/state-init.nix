{
  bounds = {
    local1 = {
      containers = ">=0.5.7.0 && <0.6";
    };
    local2 = {
      semigroups = ">=0.20";
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
      containers = {
        version = "0.5.11.0";
        hash = "158dq1h4xgqjznl4aqrb3bccm7wr20vgvg2l698b7mywfk6ssqyb";
      };
      semigroups = {
        version = "0.20";
        hash = "03llc5y6zpzfn9hy71fg2kj7pipip4fphmns9yx47xmryn111d4g";
      };
    };
  };
  lowerInit = {
    lower-local1 = {
      containers = "0.5.7.0";
    };
    lower-local2 = {
      semigroups = "0.20";
    };
  };
  resolving = false;
}
