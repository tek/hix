{ lib, config, unlines, foldMapAttrs, ... }:
with builtins;
with lib;
let
  pkgs = config.internal.basicPkgs;

  cfg = config.hackage;

  app = program: { type = "app"; inherit program; };

  allTargets = if cfg.packages == null then attrNames config.packages else cfg.packages;

  mapLines = concatMapStringsSep "\n";

  confirm = type: ''
    print -n ">>> Upload ${type} now? [yN] "
    read -q decision
    print ""
    if [[ $decision != 'y' ]]
    then
      print ">>> Aborting."
      exit
    fi
  '';

  confirmIfEnabled = type:
    if cfg.confirm then confirm type else "";

  confirmVersion = type: ''
    print ">>> Version of the uploaded packages is $version."
    ${confirmIfEnabled type}
  '';

  noVersion = type: ''
    print ">>> No version file configured, packages will be uploaded as they are."
    ${confirmIfEnabled type}
  '';

  bumpVersion = file: ''
    if [[ -z ''${new_version} ]]
    then
      current=$(sed -n 's/^version:\s*\(\S\+\)/\1/p' ${file})
      if [[ -z $current ]]
      then
        print ">>> ERROR: Could not find current version in ${file}."
        exit
      fi
      print ">>> Current version: $current"
      print -n ">>> Please enter new version (empty to keep): "
      read new_version
      version=''${new_version:-$current}
    fi
  '';

  isHpack = file: match ".*\.yaml" file != null;

  addFiles = file: ''
    git add ${file}
    git add **/*.cabal
  '';

  checkVersion = file: type: ''
    : ''${new_version:=}
    version="$new_version"
    ${if cfg.askVersion then bumpVersion file else ""}
    ${if cfg.confirm then confirmVersion type else ""}
    if [[ -n $new_version ]]
    then
      sed -i "s/^version:\(\s*\).*/version:\1$new_version/" ${file}
      ${if isHpack file then "nix run '.#hpack'" else ""}
      ${if cfg.commit && file != null then addFiles file else ""}
    fi
  '';

  handleVersion = file:
  if file != null then checkVersion file else noVersion;

  tagFragment = ''
    if [[ -n ''${version:-} ]]
    then
      git tag -m "Release $version" "v$version"
    fi
  '';

  commitFragment = ''
    if [[ -n ''${version:-} ]]
    then
      git commit --allow-empty -m "v$version"
    fi
  '';

  commitPackageFragment = name: ''
    if [[ -n ''${version:-} ]]
    then
      git commit --allow-empty -m "${name} v$version"
    fi
  '';

  command = doc: tarball: publish: target:
  let
    pkg = config.systemOutputs.packages.${target};
  in
    cfg.uploadCommand { inherit publish doc; path = tarball pkg; };

  sourceCommand =
    command false (pkg: "${pkg.release.sdist}/${pkg.pname}-${pkg.version}.tar.gz");

  docCommand =
  command true (pkg: "${pkg.release.haddock}/${pkg.pname}-${pkg.version}-docs.tar.gz");

  sourceCommands = publish:
  map (sourceCommand publish) allTargets;

  docCommands = publish:
  map (docCommand publish) allTargets;

  mkScript = name: script:
    pkgs.writeScript name ''
    #!${pkgs.zsh}/bin/zsh
    setopt err_exit no_unset
    ${script}
    '';

  readArgs = ''
  new_version=""
  while [[ -n ''${1:-} ]]
  do
    case "$1" in
      --version|-v)
        if [[ -z ''${2:-} ]]
        then
          echo "$1 needs a version argument."
          exit 1
        fi
        new_version=$2
        shift 2
        ;;
      *)
        pkg=$1
        shift
        ;;
    esac
  done
  '';

  askAndUpload = publish:
  let
    file = cfg.versionFile;
    type = if publish then "releases" else "candidates";
  in mkScript "cabal-upload-init" ''
    ${if publish && cfg.check then "nix -L flake check" else ""}
    new_version=""
    ${readArgs}
    version=$new_version
    if [[ -z ''${pkg:-} ]]
    then
      ${handleVersion file type}
      nix run .#upload-${if publish then "release" else "candidates"} $new_version
    else
      nix run .#bump-${if publish then "release" else "candidate"}-''${pkg} $new_version
    fi
    '';

  checkUploadDocs =
    mkScript "cabal-upload-docs" ''
    ${readArgs}
    if [[ -z ''${pkg:-} ]]
    then
      nix run .#upload-docs
    else
      nix run .#upload-docs-''${pkg}
    fi
    '';

  uploadAll = source: publish:
  mkScript "cabal-upload-all" (unlines (
    ["version=\${1:-}"] ++
    optionals source (sourceCommands publish) ++
    docCommands publish ++
    optional (source && publish && cfg.commit) commitFragment ++
    optional (source && publish && cfg.tag) tagFragment
  ));

  packageVersion = publish: pkg:
  let
    file = cfg.versionFiles.${pkg} or null;
    type = if publish then "release" else "candidate";
  in mkScript "cabal-upload-${pkg}" ''
  new_version=''${1:-}
  version=''${1:-}
  ${handleVersion file type}
  nix run .#upload-${if publish then "release" else "candidate"}-${pkg} $version
  '';

  uploadPackage = source: publish: name:
  mkScript "cabal-upload-package" (unlines (
    ["version=\${1:-}"] ++
    optional source (sourceCommand publish name) ++
    [(docCommand publish name)] ++
    optional (source && publish && cfg.commit) (commitPackageFragment name)
  ));

  uploadPackageApps = source: publish: type:
    let
      upload = n: app "${uploadPackage source publish n}";
      version = n: app "${packageVersion publish n}";
      target = n: {
        "upload-${type}-${n}" = upload n;
        "bump-${type}-${n}" = version n;
      };
    in foldMapAttrs target allTargets;

  uploadCandidates =
    uploadPackageApps true false "candidate";

  uploadReleases =
    uploadPackageApps true true "release";

  uploadDocs =
    uploadPackageApps false true "docs";

in {
  apps = uploadCandidates // uploadReleases // uploadDocs // {
    candidates = app "${askAndUpload false}";
    release = app "${askAndUpload true}";
    docs = app "${checkUploadDocs}";
    upload-candidates = app "${uploadAll true false}";
    upload-release = app "${uploadAll true true}";
    upload-docs = app "${uploadAll false true}";
  };
}
