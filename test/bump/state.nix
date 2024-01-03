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
      base = "4.17.2.0";
      extra = "1.7.14";
    };
    latest-other = {
      base = "4.17.2.0";
      path = "0.9.5";
    };
  };
  overrides = {
    latest-main = {
      aeson = {
        version = "2.2.0.0";
        hash = "1rxbydr7mvchhlyz2111n70db90s1zd9h6miqbbqh2kyc2l0b3pd";
      };
      bifunctors = {
        version = "5.6.1";
        hash = "12k2v7334brn4v7yg3vjd6yv6sh4pzffc3d89qgc2llw7ncv4krw";
      };
      integer-conversion = {
        version = "0.1.0.1";
        hash = "1qy49ig5k8wcqsgjf2rkbv0dy9gpbdzg8yid1kcdn0s7vys59plj";
      };
      semialign = {
        version = "1.3";
        hash = "09147lz8z79ghnpr7z08y9dd0l6c9dz9hz85avfh6z330vl25r95";
      };
      semigroupoids = {
        version = "6.0.0.1";
        hash = "1fijnbfn29iwj567kdbhk67pn11ajm80p9d3hg48hppyx7yzpb2k";
      };
      text-iso8601 = {
        version = "0.1";
        hash = "0zsqjrks9spakiwhbc7xi4dqsx68lb2cd4rvrin3iclyrqg3a6xg";
      };
      th-abstraction = {
        version = "0.5.0.0";
        hash = "0dkilfrvk8zdn3gvyfv5zgjbwqhdf1yg90fk4byka0ib43kgkyvf";
      };
    };
    latest-other = {
      path = {
        version = "0.9.5";
        hash = "05b84rizmrii847pq2fbvlpna796bwxha1vc01c3vxb2rhrknrf7";
      };
    };
  };
  initial = {
    latest-main = {
      aeson = "2.2.0.0";
      base = "4.17.2.0";
      extra = "1.7.14";
    };
    latest-other = {
      base = "4.17.2.0";
      path = "0.9.5";
    };
  };
  resolving = false;
}
