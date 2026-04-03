{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = { hix, ... }: hix.lib._hix_test {
    ghcVersions = ["ghc910"];

    packages.ps-env = {
      src = ./.;
      library.enable = true;
    };
    gen-overrides.enable = true;

    envs.profiled.enable = false;
    envs.min.enable = false;
    compat.enable = false;

    # Test envs for all combinations of decl/transform in package-set vs extra (env) overrides.
    # Each env explicitly sets the desired overrides in the package-set and env scopes, always targeting the 'some'
    # package.

    # Package sets for test envs, each extending default
    package-sets.test1.overrides = [({hackage, ...}: { some = hackage "1.0.5" "1kngk5p1nxcd69m2kma75ym61w6l09pa7260pg8vsq44cwf35ip0"; })];
    package-sets.test2.overrides = [({notest, ...}: { some = notest; })];
    package-sets.test3.overrides = [({notest, hackage, ...}: { some = notest (hackage "1.0.5" "1kngk5p1nxcd69m2kma75ym61w6l09pa7260pg8vsq44cwf35ip0"); })];
    package-sets.test4.overrides = [({notest, ...}: { some = notest; })];
    package-sets.test5.overrides = [({hackage, ...}: { some = hackage "1.0.5" "1kngk5p1nxcd69m2kma75ym61w6l09pa7260pg8vsq44cwf35ip0"; })];

    # ghc910 package-set created automatically by ghcVersions, add a decl override
    package-sets.ghc910.overrides = [({hackage, ...}: { some = hackage "1.0.5" "1kngk5p1nxcd69m2kma75ym61w6l09pa7260pg8vsq44cwf35ip0"; })];

    # test1: PS=decl, extra=transform
    envs.test1.overrides = {notest, ...}: { some = notest; };
    envs.test1.package-set.extends = "test1";

    # test2: PS=transform, extra=decl
    envs.test2.overrides = {hackage, ...}: { some = hackage "1.0.5" "1kngk5p1nxcd69m2kma75ym61w6l09pa7260pg8vsq44cwf35ip0"; };
    envs.test2.package-set.extends = "test2";

    # test3: PS=decl+transform, extra=decl – extra's decl terminates, PS decl+transform lost
    envs.test3.overrides = {hackage, ...}: { some = hackage "1.0.5" "1kngk5p1nxcd69m2kma75ym61w6l09pa7260pg8vsq44cwf35ip0"; };
    envs.test3.package-set.extends = "test3";

    # test4: PS=transform, extra=transform – both applied, no decl (falls back to super)
    envs.test4.overrides = {jailbreak, ...}: { some = jailbreak; };
    envs.test4.package-set.extends = "test4";

    # test5: PS=decl, extra=nothing – PS decl used as-is
    envs.test5.package-set.extends = "test5";

    # test6 uses the ghc910 env (auto-created via ghcVersions), PS=decl, extra=nothing
  };
}
