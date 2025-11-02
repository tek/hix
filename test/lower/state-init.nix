{
  bounds = {
    root = {
      aeson = {
        lower = "2.2.1.0";
        upper = null;
      };
      base = {
        lower = "4.17.2.1";
        upper = null;
      };
      extra = {
        lower = "1.8";
        upper = null;
      };
    };
  };
  versions = {
    latest-main = {};
    lower-main = {
      aeson = "2.2.1.0";
      base = "4.17.2.1";
      extra = "1.8";
    };
  };
  initial = {
    latest-main = {};
    lower-main = {
      aeson = "2.2.1.0";
      extra = "1.8";
    };
  };
  overrides = {
    lower-main = {
      QuickCheck = {
        version = "2.14.3";
        hash = "18451rdmih1jkrsrckdcix71zqihc4h2caic7qzizxjg4hqpapji";
        repo = "hackage.haskell.org";
      };
      aeson = {
        version = "2.2.1.0";
        hash = "1y6pc2nj4y41zmk9dsa11r2l8pdn1mr60fbmbjg25pbnzz3b38j2";
        repo = "hackage.haskell.org";
      };
      bifunctors = {
        version = "5.6.1";
        hash = "12k2v7334brn4v7yg3vjd6yv6sh4pzffc3d89qgc2llw7ncv4krw";
        repo = "hackage.haskell.org";
      };
      semialign = {
        version = "1.3";
        hash = "09147lz8z79ghnpr7z08y9dd0l6c9dz9hz85avfh6z330vl25r95";
        repo = "hackage.haskell.org";
      };
      semigroupoids = {
        version = "6.0.0.1";
        hash = "1fijnbfn29iwj567kdbhk67pn11ajm80p9d3hg48hppyx7yzpb2k";
        repo = "hackage.haskell.org";
      };
      th-abstraction = {
        version = "0.6.0.0";
        hash = "1w07ysxrbjm1rhlg9nhlq5y72s5wr4vqmcy99chvyb56wka0grbq";
        repo = "hackage.haskell.org";
      };
      time-compat = {
        version = "1.9.8";
        hash = "0xnsjpcdjms1k72b7h51i37m7qkmlrx1pw4h866p23cj2gvkf717";
        repo = "hackage.haskell.org";
        revision = true;
      };
      witherable = {
        version = "0.4.2";
        hash = "1ga4al351kwcfvsdr1ngyzj4aypvl46w357jflmgxacad8iqx4ik";
        repo = "hackage.haskell.org";
      };
    };
  };
  solver = {
    lower-main = {};
  };
  resolving = false;
}
