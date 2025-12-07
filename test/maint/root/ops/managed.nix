{
  bounds = {
    local1 = {
      base = {
        lower = null;
        upper = "5";
      };
      microlens = {
        lower = "0.4";
        upper = "0.5";
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
      microlens = {
        version = "0.4.14.0";
        hash = "sha256-mmoJty93PXFOaF/vBGL5ezAg7bXTqiTkswFyu53BeCI=";
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
    microlens = "0.4.14.0";
  };
  versions.latest-other = {
    base = "4.19.0.0";
    semigroups = "0.19.2";
  };
}
