{
  bounds = {
    root = {
      aeson = {
        lower = "1.5.6.0";
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
      aeson = "1.5.6.0";
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
        version = "1.5.6.0";
        hash = "18yb8j0jvvzp275ylj16hskgxwdy55hljch9bjvpxl25vvslnk1n";
      };
      attoparsec = {
        version = "0.14.4";
        hash = "0y9dph5axyvr1bfcvmz6qh50bjcp50m2ljra14960anc6g74a3c8";
      };
      extra = {
        version = "1.7.7";
        hash = "0jgcd8gw6d22ngbi0lp3ak2ghzza59nb3vssrjldwxiim0nzf71v";
      };
      primitive = {
        version = "0.7.4.0";
        hash = "0n7r8al9wgz4r7jzizapn1dbnkqxwx2c4lqkgfm5q5bxj8fl7g1c";
      };
      scientific = {
        version = "0.3.7.0";
        hash = "09iwj0snmx7vj7x03l4vdcn76zylcgxd9pyz0yxkydgfnn3lvc08";
      };
      th-abstraction = {
        version = "0.4.5.0";
        hash = "19nh7a9b4yif6sijp6xns6xlxcr1mcyrqx3cfbp5bdm7mkbda7a9";
      };
      vector = {
        version = "0.12.3.1";
        hash = "17jadyf0qkk1g0d1qnp8hi2aaf0ydc79ncrhswnv1dw9vrpp74w4";
      };
    };
  };
  resolving = false;
}
