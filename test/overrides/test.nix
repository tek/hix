{pkgs, ...}:
with pkgs.lib;
let
  inherit (pkgs) lib;

  ghc = pkgs.haskellPackages;
  self = ghc;
  super = ghc;

  util = import ../../lib/default.nix { inherit lib; };
  spec = import ../../lib/deps/spec.nix { inherit lib; };
  api = import ../../lib/deps/api.nix { inherit pkgs self super; };
  dep = import ../../lib/deps/default.nix { inherit pkgs; };

  v1 = api.hackage "2.1.2.1" "1f1f6h2r60ghz4p1ddi6wnq6z3i07j60sgm77hx2rvmncz4vizp0";
  v2 = api.hackage "2.0.1.0" "0nhzbnygj17m4x39mmf8r13xisc0hnkijnrwyqskf8gk276x9dpz";
  multi = api.notest v1;
  pkg = "aeson";

  args = { inherit pkgs self super pkg; };

  finalSingle = spec.reify args (spec.listOC v1);
  finalMulti = spec.reify args (spec.listOC multi);

  withOverrides =
    ghc.override { overrides = dep.reify (util.concatOverrides [(_: { aeson = v1; }) (_: { aeson = v2; })]); };

  num = n: spec.option "nums" "Test option" (toString n);

  pdecl = n: let
    impl = meta: {options, ...}: "${toString n}: ${concatStringsSep "/" options.nums}";
  in spec.decl "pure" "Test decl" { inherit n; } impl;

  poverrides = [
    (_: { test = num 1 (num 2 (pdecl 1)); })
    (_: { test = num 3 (num 4 (pdecl 2)); })
  ];

  presult = ghc.override { overrides = dep.reify (util.concatOverrides poverrides); };

in {
  test = builtins.toFile "overrides-test" ''
    cd ./dep
    nix run .#gen-overrides
    cd ../root
    flake_update

    check_eq '${finalSingle.version}' '2.1.2.1' 'Wrong version for aeson (single)'
    check_eq '${finalMulti.version}' '2.1.2.1' 'Wrong version for aeson (multi)'
    check_eq '${builtins.toJSON finalSingle.doCheck}' 'true' 'tests not enabled for single'
    check_eq '${builtins.toJSON finalMulti.doCheck}' 'false' 'tests not disabled for multi'
    check_eq '${withOverrides.aeson.version}' '2.0.1.0' 'Rightmost override doesn'''t supersede previous'
    check_eq '${presult.test}' '2: 3/4' 'Options incorrectly applied'

    error_target="The option 'gen-overrides.enable' is set, but the file 'ops/overrides.nix' doesn't exist."
    check_match_err 'nix eval .#legacyPackages.${pkgs.system}.ghc.aeson.version' $error_target 'Wrong error before gen-overrides'
    nix run .#gen-overrides
    check 'ls ops' 'overrides.nix' 'No overrides.nix in ops/'

    check 'nix eval .#legacyPackages.${pkgs.system}.ghc.aeson.version' '"2.1.2.1"' 'aeson version wrong after gen-overrides'

    nix build .#root1
    nix flake check

    sed -i 's/2\.1/5.8/' flake.nix
    error_target="Please run 'nix run .#gen-overrides' again."
    check_match_err 'nix eval .#legacyPackages.${pkgs.system}.ghc.aeson.version' $error_target 'Wrong error after changing overrides'
  '';
}
