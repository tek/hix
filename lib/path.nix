{util}: let

  inherit (util) internal lib project;

  noRoot = message: throw ''
  Could not determine the project's root directory in the store.

  ${message}
  See https://hix.how#packages for more information.
  '';

  explicitInstructions = pkg: ''
  If this is intended, you need to explicitly configure these options:
    base = ./.;
    packages.${pkg}.relativePath = "path/to/package";
  '';

  badSrc = name: ''
  The 'src' option for the package '${name}' is neither a store path nor a subdirectory of a root that contains a 'flake.nix'.
  ${explicitInstructions name}'';

  noPackages = ''
  The configuration does not contain any packages, whose 'src' option is used to infer the root.
  Either configure a package, or set the 'base' option manually:
    base = ./.;
  '';

  tranformationExplanation = ''
  This may happen when source filters or other transformations are applied to these paths.
  '';

  incoherentSrcs = ''
  The 'src' options in some of the packages refer to different store paths than others.
  ${tranformationExplanation}${explicitInstructions "*"}'';

  separateSrc = name: ''
  The 'src' option for the package '${name}' is not a subdirectory of the project root in the store.
  ${tranformationExplanation}${explicitInstructions name}'';

  findRoot = name: path: let

    step = p:
      if p == "/"
      then noRoot badSrc
      else if lib.isStorePath p || lib.pathExists "${p}/flake.nix"
      then p
      else step (dirOf p);

  in step path;

  packageRoots = lib.mapAttrs findRoot internal.project.packagePaths;

  inferBase = let
    roots = lib.attrValues packageRoots;
    root = lib.head roots;
  in
  if lib.length roots == 0
  then noRoot noPackages
  else if lib.all (p: p == root) (lib.tail roots)
  then root
  else noRoot incoherentSrcs;

  relative = specRaw:
  let
    base = project.base;
    spec = toString specRaw;
    new = lib.strings.removePrefix (toString base + "/") spec;
    failed = new == spec;
    specIsAbsolute = builtins.isPath specRaw || builtins.substring 0 1 spec == "/";
    isRoot = spec == toString base;
    withAbsolute =
      if isRoot
      then "."
      else if failed
      then throw separateSrc
      else new
      ;
  in
  if specIsAbsolute
  then withAbsolute
  else spec;

in {
  inherit relative inferBase packageRoots;
}
