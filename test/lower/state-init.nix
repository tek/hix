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
        repo = "hackage.haskell.org";
      };
      extra = {
        version = "1.7.7";
        hash = "0jgcd8gw6d22ngbi0lp3ak2ghzza59nb3vssrjldwxiim0nzf71v";
        repo = "hackage.haskell.org";
      };
      integer-conversion = {
        version = "0.1.0.1";
        hash = "1qy49ig5k8wcqsgjf2rkbv0dy9gpbdzg8yid1kcdn0s7vys59plj";
        repo = "hackage.haskell.org";
      };
      network-uri = {
        version = "2.6.4.2";
        hash = "0zj83viziy80f7nybpmc1hki8wrd8pzps31fxns9vxhc1p7l9chj";
        repo = "hackage.haskell.org";
      };
      text-iso8601 = {
        version = "0.1";
        hash = "0zsqjrks9spakiwhbc7xi4dqsx68lb2cd4rvrin3iclyrqg3a6xg";
        repo = "hackage.haskell.org";
      };
      th-compat = {
        version = "0.1.4";
        hash = "1h5p7hgjhrrkr2k97c1wmv6lhw9zs8wkq7ja2r2119va55k2ldwz";
        repo = "hackage.haskell.org";
      };
    };
  };
  solver = {
    lower-main = {};
  };
  resolving = false;
}
