{pkgs, ...}:
with pkgs.lib;
let
  inherit (pkgs) lib;

  ghc = pkgs.haskellPackages;
  self = ghc;
  super = ghc;

  util = import ../../lib/default.nix { inherit lib; };
  spec = import ../../lib/deps/spec.nix { inherit lib; };
  api = import ../../lib/deps/api.nix { inherit pkgs; } { inherit self super; };
  dep = import ../../lib/deps/default.nix { } { inherit pkgs; };

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
  source = ''
    pushd ../dep
    nix run .#gen-overrides
    popd

    describe "Version for $(yellow aeson) (single)"
    output_exact '2.1.2.1'
    step print ${finalSingle.version}

    describe "Version for $(yellow aeson) (multi)"
    output_exact '2.1.2.1'
    step print ${finalMulti.version}

    describe 'Tests enabled for single'
    output_exact 'true'
    step print ${builtins.toJSON finalSingle.doCheck}

    describe 'Tests disabled for multi'
    output_exact 'false'
    step print ${builtins.toJSON finalMulti.doCheck}

    describe 'Rightmost override supersedes previous'
    output_exact '2.0.1.0'
    step print ${withOverrides.aeson.version}

    describe 'Options correctly applied'
    output_exact '2: 3/4'
    step print ${presult.test}

    aeson_version="legacyPackages.${pkgs.system}.env.dev.ghc.aeson.version"

    describe 'Error message before gen-overrides'
    error_match "The option 'gen-overrides.enable' is set, but the file 'ops/overrides.nix' doesn't exist."
    exit_code 1
    step_eval $aeson_version

    step_run gen-overrides

    describe "$(color_path overrides.nix) exists in $(color_path ops/)"
    output_exact 'overrides.nix'
    step ls ops

    describe "$(yellow aeson) version after gen-overrides"
    output_exact '"2.1.2.1"'
    step_eval $aeson_version

    step_build root1

    error_ignore
    step_nix flake check

    step sed -i 's/2\.1/5.8/' flake.nix

    describe 'Error message after changing overrides'
    error_match "Please run 'nix run .#gen-overrides' again."
    exit_code 1
    step_eval $aeson_version
  '';
}
