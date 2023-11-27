{config, lib, util}: let

  deps = import ./deps/default.nix { inherit (config) pkgs; };

  extraHs = env: ghc:
  if lib.isFunction env.haskellPackages
  then env.haskellPackages ghc
  else map (n: ghc.${n}) env.haskellPackages;

  packageDb = env: ghc: let
    targets = util.env.targets env;
    bInputs = p: p.buildInputs ++ p.propagatedBuildInputs;
    isNotLocal = p: !(p ? pname && lib.elem p.pname targets);
    localDeps = builtins.filter isNotLocal (lib.concatMap bInputs (map (p: ghc.${p}) targets));
  in lib.optionals env.localDeps localDeps ++ extraHs env ghc ++ [ghc.cabal-install];

  local = env: let
    overrides =
      util.concatOverrides [
        (util.overridesFromDeps ["local"])
        env.internal.overridesLocal
      ];
  in env.ghc.vanillaGhc.override { overrides = deps.reify overrides; };

  packageDbLocal = env: (local env).ghcWithPackages (packageDb env);

  packageDbFull = env: args: env.ghc.ghc.ghcWithPackages.override args (packageDb env);

in {
  inherit packageDb local packageDbLocal packageDbFull;
}
