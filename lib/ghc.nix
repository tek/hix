{config, lib, util}: let

  deps = import ./deps/default.nix { inherit (config) pkgs; };

  extraHs = env: ghc:
  if lib.isFunction env.haskellPackages
  then env.haskellPackages ghc
  else map (n: ghc.${n}) env.haskellPackages;

  packageDb = noLocalsInDeps: env: ghc: let
    targets = util.env.targets env;
    unwanted = if noLocalsInDeps then config.internal.packageNames else targets;
    bInputs = p: p.buildInputs ++ p.propagatedBuildInputs;
    isWanted = p: !(p ? pname && lib.elem p.pname unwanted);
    targetDeps = builtins.filter isWanted (lib.concatMap bInputs (map (p: ghc.${p}) targets));
  in lib.optionals env.localDeps targetDeps ++ extraHs env ghc ++ [ghc.cabal-install];

  local = env: let
    overrides =
      util.concatOverrides [
        (util.overridesFromDeps ["local"])
        env.internal.overridesLocal
      ];
  in env.ghc.vanillaGhc.override { overrides = deps.reify overrides; };

  packageDbVanilla = env: (local env).ghcWithPackages (packageDb true env);

  packageDbLocal = env: (local env).ghcWithPackages (packageDb false env);

  packageDbFull = env: args: env.ghc.ghc.ghcWithPackages.override args (packageDb false env);

in {
  inherit packageDb local packageDbVanilla packageDbLocal packageDbFull;
}
