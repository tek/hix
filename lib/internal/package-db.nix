{util}: let

  inherit (util) config lib internal;

  deps = import ../deps/default.nix { inherit config; };

  extraHs = env: ghc:
  if lib.isFunction env.haskellPackages
  then env.haskellPackages ghc
  else map (n: ghc.${n}) env.haskellPackages;

  # Collect the target dependencies of the specified env from the specified GHC package set.
  # If noLocalsInDeps is `true`, exclude dependencies if they are local packages.
  # If it is `false`, exclude dependencies if they are env target packages, but not if they are non-target local
  # packages (i.e. targets of a different env).
  packageDb = noLocalsInDeps: env: ghc: let
    targets = util.internal.env.targets env;
    unwanted = if noLocalsInDeps then internal.project.packageNames else targets;
    bInputs = p: p.buildInputs ++ p.propagatedBuildInputs;
    isWanted = p: !(p ? pname && lib.elem p.pname unwanted);
    targetDeps = lib.filter isWanted (lib.filter (a: a != null) (lib.concatMap bInputs (map (p: ghc.${p}) targets)));
  in lib.optionals env.localDeps targetDeps ++ extraHs env ghc;

  solverGhc = env: let
    overrides =
      util.concatOverrides [
        (util.overridesFromDeps ["local"])
        env.internal.overridesLocal
        env.internal.overridesSolver
        (internal.env.managedSolverOverrides env.name)
      ];
  in env.toolchain.vanilla.override { overrides = (deps { inherit (env.toolchain) pkgs; }).reify overrides; };

  solver = noLocalsInDeps: env: (solverGhc env).ghcWithPackages (packageDb noLocalsInDeps env);

  full = env: args: env.toolchain.packages.ghcWithPackages.override args (packageDb false env);

in {
  inherit packageDb solver full;
}
