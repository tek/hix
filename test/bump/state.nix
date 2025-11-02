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
      local1 = {
        lower = null;
        upper = null;
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
        upper = "4.20";
      };
      local2 = {
        lower = null;
        upper = null;
      };
      path = {
        lower = null;
        upper = "0.10";
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
      path = "0.9.5";
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
        repo = "hackage.haskell.org";
      };
    };
    latest-other = {
      path = {
        version = "0.9.5";
        hash = "05b84rizmrii847pq2fbvlpna796bwxha1vc01c3vxb2rhrknrf7";
        repo = "hackage.haskell.org";
      };
    };
  };
  solver = {
    latest-main = {};
    latest-other = {};
  };
  resolving = false;
}
