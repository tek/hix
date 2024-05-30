{
  bounds = {
    root = {
      aeson = {
        lower = "2.2.0.0";
        upper = null;
      };
      base = {
        lower = "4.15.1.0";
        upper = null;
      };
      extra = {
        lower = "1.7.7";
        upper = null;
      };
    };
  };
  versions = {
    latest-main = {};
    lower-main = {
      aeson = "2.2.0.0";
      base = "4.15.1.0";
      extra = "1.7.7";
    };
  };
  initial = {
    latest-main = {};
    lower-main = {
      aeson = "2.2.0.0";
      extra = "1.7.7";
    };
  };
  overrides = {
    lower-main = {
      aeson = {
        version = "2.2.0.0";
        hash = "1rxbydr7mvchhlyz2111n70db90s1zd9h6miqbbqh2kyc2l0b3pd";
      };
      extra = {
        version = "1.7.7";
        hash = "0jgcd8gw6d22ngbi0lp3ak2ghzza59nb3vssrjldwxiim0nzf71v";
      };
      integer-conversion = {
        version = "0.1.0.1";
        hash = "1qy49ig5k8wcqsgjf2rkbv0dy9gpbdzg8yid1kcdn0s7vys59plj";
      };
      text-iso8601 = {
        version = "0.1.1";
        hash = "0lk8y0g5p2zamkgvxy7i5lqfpa4wnic9vg2bl87s801fniwdij3m";
      };
    };
  };
  resolving = false;
}
