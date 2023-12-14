{
  bounds = {
    local1 = {
      aeson = ">=2.2 && <2.3";
      extra = ">=1.7 && <1.8";
      th-abstraction = ">=0.5 && <0.6";
    };
    local2 = {
      path = ">=0.9 && <0.10";
    };
  };
  overrides = {
    latest-main = {
      aeson = {
        version = "2.2.0.0";
        hash = "1rxbydr7mvchhlyz2111n70db90s1zd9h6miqbbqh2kyc2l0b3pd";
      };
      extra = {
        version = "1.7.14";
        hash = "0dvp4h3grnfkly6aj7j9ic62qgy4gzvn8c8s8d4ncx6kjglgwn3v";
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
  lowerInit = {};
  resolving = false;
}
