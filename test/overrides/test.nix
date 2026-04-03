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
  tests = {

    root = {
      source = ''
        pushd ../dep
        step_run gen-overrides
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

        aeson="legacyPackages.${pkgs.system}.env.dev.ghc.aeson"
        aeson_version="''${aeson}.version"

        describe 'Error message before gen-overrides'
        error_match "The option 'gen-overrides.enable' is set, but the file 'ops/overrides.nix' doesn't exist."
        exit_code 1
        step_eval $aeson_version

        step_run gen-overrides

        describe "$(color_path overrides.nix) exists in $(color_path ops/)"
        output_exact 'overrides.nix'
        step ls ops

        describe "$(yellow aeson) version after gen-overrides"
        output_exact '"2.2.2.0"'
        step_eval $aeson_version

        describe "$(yellow aeson) revision after gen-overrides"
        output_exact '"1"'
        step_eval ''${aeson}.passthru.revision

        step_build root1

        error_ignore
        step_nix flake check

        step sed -i 's/2\.2/5.8/' flake.nix

        describe 'Error message after changing overrides'
        error_match "Please run 'nix run .#gen-overrides' again."
        exit_code 1
        step_eval $aeson_version
      '';
    };

    ps-env = {
      source = ''
        step_run gen-overrides

        test_some() {
          echo "legacyPackages.${pkgs.system}.env.$1.ghc.some"
        }

        # test1: PS=decl, extra=transform
        test1=$(test_some test1)

        describe 'test1 (PS=decl, extra=transform): hackage version'
        output_exact '"1.0.5"'
        step_eval ''${test1}.version

        describe 'test1 (PS=decl, extra=transform): notest applied'
        output_exact 'false'
        step_eval ''${test1}.doCheck

        # test2: PS=transform, extra=decl
        # Standard compile semantics: extra's decl terminates compilation, PS's transform is lost
        test2=$(test_some test2)

        describe 'test2 (PS=transform, extra=decl): hackage version'
        output_exact '"1.0.5"'
        step_eval ''${test2}.version

        describe 'test2 (PS=transform, extra=decl): notest not applied (decl terminates)'
        output_exact 'true'
        step_eval ''${test2}.doCheck

        # test3: PS=decl+transform, extra=decl — extra's decl terminates, PS decl+transform lost
        test3=$(test_some test3)

        describe 'test3 (PS=decl+transform, extra=decl): hackage version'
        output_exact '"1.0.5"'
        step_eval ''${test3}.version

        describe 'test3 (PS=decl+transform, extra=decl): PS notest lost (decl terminates)'
        output_exact 'true'
        step_eval ''${test3}.doCheck

        # test4: PS=transform, extra=transform — both applied, falls back to super
        test4=$(test_some test4)

        describe 'test4 (PS=transform, extra=transform): notest applied'
        output_exact 'false'
        step_eval ''${test4}.doCheck

        # test5: PS=decl, extra=nothing — PS decl used as-is
        test5=$(test_some test5)

        describe 'test5 (PS=decl, extra=nothing): hackage version'
        output_exact '"1.0.5"'
        step_eval ''${test5}.version

        describe 'test5 (PS=decl, extra=nothing): doCheck unmodified'
        output_exact 'true'
        step_eval ''${test5}.doCheck

        # test6: ghcVersion env (ghc910), PS=decl, extra=nothing — PS decl propagated via extends
        test6=$(test_some ghc910)

        describe 'test6 (ghc910 PS=decl, extra=nothing): hackage version'
        output_exact '"1.0.5"'
        step_eval ''${test6}.version

        describe 'test6 (ghc910 PS=decl, extra=nothing): doCheck unmodified'
        output_exact 'true'
        step_eval ''${test6}.doCheck
      '';
    };

  };
}

