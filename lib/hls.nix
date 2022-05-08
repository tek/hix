{ config, overrides }:
let
    hlsOverrides = { hackage, jailbreak, ... }: {
      ghcide = hackage "1.7.0.0" "14nyh5if69n8803ig8p438616bkww0x1i7cl9ibf282ylhwmq84b";
      haskell-language-server = hackage "1.7.0.0" "1qh4vypax63d5l26sz0ls05xbd86pqq4ixdcdhysgqq0pwjna5hx";
      hls-alternate-number-format-plugin = hackage "1.1.0.0" "0f84xxkv5pfx0wbklg33a1sz4j2mcdfwl164vzrh02ws7zy7j78h";
      hls-brittany-plugin = hackage "1.0.2.1" "0kk66mzfly6mifj7f8290jlf0zs4g8zgk1vyp83wscql284k49fn";
      hls-call-hierarchy-plugin = hackage "1.0.3.0" "1d7yim9ggc6d8496vzsx8vg9a8yvf4jyiv1rqnlmf0g11ph3n2zg";
      hls-change-type-signature-plugin = hackage "1.0.1.0" "0dw9c9kqh2mwrlniwlvjp7vaff35k9kfc90l5g8xv9sv214kfign";
      hls-class-plugin = hackage "1.0.3.0" "125zzz79lfzc9d4gb6iw91k2q78jzc4m0ia8xg9vbhxxxaaz9mfi";
      hls-eval-plugin = hackage "1.2.2.0" "0rhf0yy2sw7fik8wrslgpvwh9xhp2ziv13b8b0if2aq4q9x84ljy";
      hls-explicit-imports-plugin = hackage "1.1.0.0" "1xsamhx91ivhp2ryf0ndx7cdzr15zlqvrkv7n7wdxy93aspql7fi";
      hls-floskell-plugin = hackage "1.0.1.1" "1bs0dj02347g2zm0gxzpyw4xknfpcb3d1i0pv502l1slha2k5d02";
      hls-fourmolu-plugin = hackage "1.0.3.0" "0j0mj70kjiymm3v7cwg1d76kck5j4yrsxpyapikvzqnd33n0pkvl";
      hls-graph = hackage "1.7.0.0" "1xjjyxd7a6szs86v1h51fw0k8criv12ffbpn1wqxkvw1lcxszviy";
      hls-haddock-comments-plugin = jailbreak (hackage "1.0.1.0" "11r20izq4d9pkils8kkp9yzf0rp29n0k8k2frfpb98nh1qcq4vid");
      hls-hlint-plugin = hackage "1.0.4.0" "0wa1vhcrxbqmia09gnn7rvi1pn680pd6b0hx73pi5ki7cb6nc69q";
      hls-module-name-plugin = hackage "1.0.2.0" "0dbc53x6x1prrv10c18j8jg162z6q8f338prwx50p2xlzgbpvqxr";
      hls-ormolu-plugin = hackage "1.0.2.1" "1k6imb83sylkw167sms48b72qs2fl0yllpzgybxw719nwff9ihw6";
      hls-plugin-api = hackage "1.4.0.0" "0vzikzs37yksv249pzmb77zqx0a40m7wm29yfli5zcw0adnwh0vs";
      hls-pragmas-plugin = hackage "1.0.2.1" "0rsjqi1xqfdrdwmmna4hnk8hki61w92ig981zbsd0nzlmlskb1iy";
      hls-qualify-imported-names-plugin = jailbreak (hackage "1.0.1.0" "09mjphrs2wcxb1mwcmc7lh7ai4qk8xyvq9s6zcvg4gd6rlgxlqj8");
      hls-refine-imports-plugin = hackage "1.0.2.0" "0vmq0qy0g71il69hgcyp4mmilfz446n40bwy9vf2crmnlywiv4bw";
      hls-rename-plugin = hackage "1.0.0.2" "1a315ci4yaiv82vp2gwmv1189ykx0xd7rhrgkfyxm6xyldd2qwl6";
      hls-retrie-plugin = hackage "1.0.2.1" "0bzzf105bbwsgv0iqws2kkc8y1n6h94d31zl1f05ag9819k9vvjc";
      hls-selection-range-plugin = jailbreak (hackage "1.0.0.0" "0pq1823zd19syp5qzlxra9c6ka857hdmxiiw3v48j1xdgmx5hag4");
      hls-splice-plugin = jailbreak (hackage "1.0.1.0" "0izgb8jfr1kd2y54av75wlrdc5qwrjb0dq08lgspsfap3syllmid");
      hls-stylish-haskell-plugin = hackage "1.0.1.1" "0ykvs43wkyszmxnfbcjg17qvkkqqc2zg2ydr60kfpz3abka2mny2";
      hls-tactics-plugin = hackage "1.6.2.0" "0gnvrckr8yiyxgaf95i3r4xq1qw1mf630fwzgnjw11vjacvspm8n";
    };
in
  (overrides.ghcWithOverrides [hlsOverrides] config.devGhc.vanillaGhc).haskell-language-server
