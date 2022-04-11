{ obelisk }:
{ hackage, source, configure, override, pkgs, minimal, ... }:
{
  # reflex
  universe = hackage "1.2.2" "1kdl05l8xfl3439zzmskzka9dffhip8j07zqcdhq28rbb194rra5";
  universe-base = hackage "1.1.3" "1qj1cljwwplkp3x12pljww3jaswijwhxfpmbdbif38ifc9bzvfgx";
  universe-dependent-sum = hackage "1.3" "17sjgc9g2zxg5bgcqkr9i7cwcg9a8w4w6f2sssww9sr97k5f71js";
  universe-instances-extended = hackage "1.1.3" "1lb9mhsdynsinwis4738mx77qc11lrjb9kv3gafvp6f69ch7av62";
  universe-reverse-instances = hackage "1.1.1" "1nd12c7q656bwb51mwsdb58qs9gs25ap2vlblfp102bfbanjlwfn";
  universe-instances-base = hackage "1.1" "1d5hrbn6ydyhkf9i9497rpixrcydryx6il65q4jwjqvrqpnrwgv5";

  # obelisk
  obelisk-backend = source.sub obelisk "lib/backend";
  obelisk-frontend = source.sub obelisk "lib/frontend";
  obelisk-route = source.sub obelisk "lib/route";
  obelisk-run = override { librarySystemDepends = [pkgs.iproute]; } (source.sub obelisk "lib/run");
  obelisk-asset-serve-snap = source.sub obelisk "lib/asset/serve-snap";
  obelisk-executable-config-inject = source.sub obelisk "lib/executable-config/inject";
  obelisk-executable-config-lookup = source.sub obelisk "lib/executable-config/lookup";
  obelisk-snap-extras = source.sub obelisk "lib/snap-extras";
  tabulation = source.sub obelisk "lib/tabulation";
}
