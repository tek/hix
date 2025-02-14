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
      extra = {
        lower = null;
        upper = "1.8";
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
    local3 = {
      base = {
        lower = null;
        upper = null;
      };
      path = {
        lower = null;
        upper = null;
      };
    };
  };
  versions = {
    latest-main = {
      aeson = "2.2.3.0";
      base = "4.19.2.0";
      extra = "1.7.14";
    };
    latest-other = {
      base = "4.19.2.0";
    };
  };
  initial = {
    latest-main = {};
    latest-other = {};
  };
  overrides = {
    latest-main = {
      extra = {
        version = "1.7.14";
        hash = "0dvp4h3grnfkly6aj7j9ic62qgy4gzvn8c8s8d4ncx6kjglgwn3v";
      };
    };
  };
  resolving = false;
}
