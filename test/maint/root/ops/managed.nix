{
  bounds = {
    local1 = {
      base = {
        lower = null;
        upper = "5";
      };
      extra = {
        lower = "1.7";
        upper = "1.8";
      };
    };
    local2 = {
      base = {
        lower = null;
        upper = "5";
      };
      semigroups = {
        lower = "0.19";
        upper = "0.20";
      };
    };
  };
  overrides = {
    latest-main = {
      extra = {
        version = "1.7.16";
        hash = "0y27q0zas66qbgvjga0w8dmfjcs0kcn9nwps17iyd60ij3yqivhj";
      };
    };
    latest-other = {
      semigroups = {
        version = "0.19.2";
        hash = "1q8ssd9c3rr1d0vjc6vdnl27r9n0p3xxacag6s0ahsa2njz225ra";
      };
    };
  };
  versions.latest-main = {
    base = "4.19.0.0";
    extra = "1.7.16";
  };
  versions.latest-other = {
    base = "4.19.0.0";
    semigroups = "0.19.2";
  };
}
