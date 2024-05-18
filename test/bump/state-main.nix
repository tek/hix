{
  bounds = {
    local1 = {
      aeson = {
        lower = "2.2";
        upper = "2.3";
      };
      base = {
        lower = null;
        upper = "4.18";
      };
      extra = {
        lower = null;
        upper = "1.8";
      };
    };
    local2 = {
      base = {
        lower = null;
        upper = "4.18";
      };
      path = {
        lower = null;
        upper = "0.10";
      };
    };
  };
  versions = {
    latest-main = {
      aeson = "2.2.0.0";
      base = "4.17.2.1";
      extra = "1.7.14";
    };
    latest-other = {
      base = "4.17.2.1";
    };
  };
  initial = {
    latest-main = {};
    latest-other = {};
  };
  overrides = {
    latest-main = {
      aeson = {
        version = "2.2.0.0";
        hash = "1rxbydr7mvchhlyz2111n70db90s1zd9h6miqbbqh2kyc2l0b3pd";
      };
      integer-conversion = {
        version = "0.1.0.1";
        hash = "1qy49ig5k8wcqsgjf2rkbv0dy9gpbdzg8yid1kcdn0s7vys59plj";
      };
      text-iso8601 = {
        version = "0.1";
        hash = "0zsqjrks9spakiwhbc7xi4dqsx68lb2cd4rvrin3iclyrqg3a6xg";
      };
    };
  };
  resolving = false;
}
