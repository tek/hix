{
  bounds = {
    local1 = {
      aeson = {
        lower = "2.2";
        upper = "2.3";
      };
      base = {
        lower = null;
        upper = "4.20";
      };
    };
    local2 = {
      base = {
        lower = null;
        upper = "4.20";
      };
      path = {
        lower = null;
        upper = "0.10";
      };
    };
  };
  versions.latest-main = {
    aeson = "2.1.0.0";
    base = "4.19.2.0";
  };
  versions.latest-other = {
    base = "4.19.2.0";
  };
}
