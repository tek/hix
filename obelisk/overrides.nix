inputs:
{ hackage, source, configure, override, pkgs, minimal, ... }:
{
  # reflex
  universe = hackage "1.2.1" "00876daw7xf1w33szp81r0zp9y6xy6j5pa9i6pvrsf1xybihd6s2";
  universe-base = hackage "1.1.2" "1k2fizxs7skv36bw7m3d70aj7jp45cprh2s4z4y3sc04if2xcjxc";
  universe-dependent-sum = hackage "1.3" "17sjgc9g2zxg5bgcqkr9i7cwcg9a8w4w6f2sssww9sr97k5f71js";
  universe-instances-extended = hackage "1.1.2" "1ilbr6xm5lrvyvg6zsg2manw1h505pjzv5vmymmdziqfys043jc9";
  universe-reverse-instances = hackage "1.1.1" "1nd12c7q656bwb51mwsdb58qs9gs25ap2vlblfp102bfbanjlwfn";
  universe-instances-base = hackage "1.1" "1d5hrbn6ydyhkf9i9497rpixrcydryx6il65q4jwjqvrqpnrwgv5";

  # obelisk
  obelisk-backend = source.sub inputs.obelisk "lib/backend";
  obelisk-frontend = source.sub inputs.obelisk "lib/frontend";
  obelisk-route = source.sub inputs.obelisk "lib/route";
  obelisk-run = override { librarySystemDepends = [pkgs.iproute]; } (source.sub inputs.obelisk "lib/run");
  obelisk-asset-serve-snap = source.sub inputs.obelisk "lib/asset/serve-snap";
  obelisk-executable-config-inject = source.sub inputs.obelisk "lib/executable-config/inject";
  obelisk-executable-config-lookup = source.sub inputs.obelisk "lib/executable-config/lookup";
  obelisk-snap-extras = source.sub inputs.obelisk "lib/snap-extras";
  tabulation = source.sub inputs.obelisk "lib/tabulation";
}
