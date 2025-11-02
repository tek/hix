{
  bounds = {
    root = {
      aeson = {
        lower = "2.1.2.1";
        upper = null;
      };
      base = {
        lower = "4.17.2.1";
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
      aeson = "2.1.2.1";
      base = "4.17.2.1";
      extra = "1.7.7";
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
        version = "2.1.2.1";
        hash = "1f1f6h2r60ghz4p1ddi6wnq6z3i07j60sgm77hx2rvmncz4vizp0";
        repo = "hackage.haskell.org";
      };
      attoparsec = {
        version = "0.14.4";
        hash = "0y9dph5axyvr1bfcvmz6qh50bjcp50m2ljra14960anc6g74a3c8";
        repo = "hackage.haskell.org";
      };
      base-compat = {
        version = "0.13.1";
        hash = "19pgyjgwx81vlyl8c0rlmfcympzy38bsmjq238dn8fv5b03jck4r";
        repo = "hackage.haskell.org";
      };
      base-compat-batteries = {
        version = "0.13.1";
        hash = "06fq90abnlqdsa7iqfmp1fndldmfdfjvx8n8s963nnvnmq6hxs4g";
        repo = "hackage.haskell.org";
      };
      bifunctors = {
        version = "5.6.1";
        hash = "12k2v7334brn4v7yg3vjd6yv6sh4pzffc3d89qgc2llw7ncv4krw";
        repo = "hackage.haskell.org";
      };
      extra = {
        version = "1.7.7";
        hash = "0jgcd8gw6d22ngbi0lp3ak2ghzza59nb3vssrjldwxiim0nzf71v";
        repo = "hackage.haskell.org";
      };
      indexed-traversable-instances = {
        version = "0.1.1.2";
        hash = "1mmkklfpagv855p12dqq0r6xwg0v6dc1gj1n3nvzzy4b909ajgd0";
        repo = "hackage.haskell.org";
      };
      primitive = {
        version = "0.8.0.0";
        hash = "0y8yw8fnfh4dg6yjny2y8b4pmvvhhr7611j2z7i1dnl8w8cvbmb3";
        repo = "hackage.haskell.org";
      };
      scientific = {
        version = "0.3.7.0";
        hash = "09iwj0snmx7vj7x03l4vdcn76zylcgxd9pyz0yxkydgfnn3lvc08";
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
        version = "0.5.0.0";
        hash = "0dkilfrvk8zdn3gvyfv5zgjbwqhdf1yg90fk4byka0ib43kgkyvf";
        repo = "hackage.haskell.org";
      };
      time-compat = {
        version = "1.9.8";
        hash = "0xnsjpcdjms1k72b7h51i37m7qkmlrx1pw4h866p23cj2gvkf717";
        repo = "hackage.haskell.org";
        revision = true;
      };
      vector = {
        version = "0.13.1.0";
        hash = "0c1nw2sx14y29afdbwl40sk9vznx71rja5jcg14b8986778kl32d";
        repo = "hackage.haskell.org";
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
