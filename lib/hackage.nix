{ lib, config, util, ... }:
with builtins;
with lib;
let
  inherit (util) app;

  pkgs = config.internal.pkgs;

  git = "${pkgs.git}/bin/git";

  cfg = config.hackage;

  gitAdd = cfg.commit || cfg.add;

  allTargets = if cfg.packages == null then config.internal.packageNames else cfg.packages;

  allCabals = concatMapStringsSep " " (n: "${config.internal.relativePackages.${n}}/${n}.cabal") allTargets;

  confirm = type: ''
    print -n ">>> Upload ${type} now? [yN] "
    read -q decision || true
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
      current="$(${cfg.versionFileExtract file})"
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

  reuseVersion = file: ''
    if [[ -z $version ]]
    then
      version="$(${cfg.versionFileExtract file})"
    fi
  '';

  needsGenCabal = file: match ".*\.(yaml|nix)" file != null;

  addFiles = file: ''
    ${git} add ${if file == null then "" else file} ${allCabals}
  '';

  bumpChangelogs = ''
    changelogs=$(${pkgs.fd}/bin/fd -i 'changelog(\..*)?')
    if [[ -n $changelogs ]]
    then
      sed -i "s/Unreleased/$new_version/" $=changelogs
      ${optionalString gitAdd "${git} add $=changelogs"}
    fi
  '';

  checkVersion = file: type: ''
    : ''${new_version:=}
    version="$new_version"
    ${if cfg.askVersion then bumpVersion file else reuseVersion file}
    ${if cfg.confirm then confirmVersion type else ""}
    if [[ -n $new_version ]]
    then
      ${cfg.versionFileUpdate file}
      ${if needsGenCabal file then "nix run '.#gen-cabal-quiet'" else ""}
      ${if gitAdd && file != null then addFiles file else ""}
      ${if cfg.setChangelogVersion then bumpChangelogs else ""}
    fi
  '';

  handleVersion = file:
  if file != null then checkVersion file else noVersion;

  formatTag = name: cfg.formatTag { inherit name; version = "$version"; };

  tagFragment = ''
    if [[ -n ''${version:-} ]]
    then
      ${git} tag ${cfg.tagExtraArgs} -m "Release $version" "${formatTag null}"
    fi
  '';

  commitFragment = ''
    if [[ -n ''${version:-} ]]
    then
      ${cfg.hooks.preCommitAll}
      ${git} commit ${cfg.commitExtraArgs} --allow-empty -m "Release $version"
      ${cfg.hooks.postCommitAll}
    fi
  '';

  tagPackageFragment = name: ''
    if [[ -n ''${version:-} ]]
    then
      ${git} tag ${cfg.tagExtraArgs} -m "Release ${name} $version" "${formatTag name}"
    fi
  '';

  commitPackageFragment = name: ''
    if [[ -n ''${version:-} ]]
    then
      ${git} commit ${cfg.commitExtraArgs} --allow-empty -m "Release ${name} $version"
    fi
  '';

  command = doc: tarball: publish: target:
  let
    pkg = config.output.final.packages.${target};
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
    util.zscript name ''
    setopt no_unset
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
    new_version=""
    ${readArgs}
    version=$new_version
    if [[ -z ''${pkg:-} ]]
    then
      ${handleVersion file type}
      ${if publish && cfg.check then "nix flake check" else ""}
      ${util.runBuildApp "hackage.upload.${if publish then "release" else "candidates"}"} $version
    else
      ${util.runBuildApp ''hackage.bump.${if publish then "release" else "candidate"}-''${pkg}''} $version
    fi
    '';

  checkUploadDocs =
    mkScript "cabal-upload-docs" ''
    ${readArgs}
    if [[ -z ''${pkg:-} ]]
    then
      ${util.runBuildApp "hackage.upload.docs"}
    else
      ${util.runBuildApp ''hackage.upload.docs-''${pkg}''}
    fi
    '';

  uploadAll = source: publish: let
    hookArgs = { inherit source publish; };
  in mkScript "cabal-upload-all" (util.unlines (
    ["version=\${1:-}"] ++
    optionals source (sourceCommands publish) ++
    docCommands publish ++
    [(cfg.hooks.postUploadAll hookArgs)] ++
    optional (source && publish && cfg.commit) commitFragment ++
    optional (source && publish && cfg.tag) tagFragment
  ));

  packageVersion = publish: pkg:
  let
    file = config.packages.${pkg}.versionFile;
    type = if publish then "release" else "candidate";
  in mkScript "cabal-upload-${pkg}" ''
  new_version=''${1:-}
  version=''${1:-}
  ${handleVersion file type}
  ${if publish && cfg.check then "nix -L flake check" else ""}
  ${util.runBuildApp "hackage.upload.${if publish then "release" else "candidate"}-${pkg}"} $version
  '';

  uploadPackage = source: publish: name:
  mkScript "cabal-upload-package" (util.unlines (
    ["version=\${1:-}"] ++
    optional source (sourceCommand publish name) ++
    [(docCommand publish name)] ++
    optional (source && publish && cfg.commit) (commitPackageFragment name) ++
    optional (source && publish && cfg.tag) (tagPackageFragment name)
  ));

  uploadPackageApps = source: publish: type:
    let
      upload = n: app "${uploadPackage source publish n}";
      version = n: app "${packageVersion publish n}";
      target = n: {
        upload = { "${type}-${n}" = upload n; };
        bump = { "${type}-${n}" = version n; };
      };
    in { hackage = util.mergeAllAttrs (map target allTargets); };

  uploadCandidates =
    uploadPackageApps true false "candidate";

  uploadReleases =
    uploadPackageApps true true "release";

  uploadDocs =
    uploadPackageApps false true "docs";

  mainApps = {
    candidates = app "${askAndUpload false}";
    release = app "${askAndUpload true}";
    docs = app "${checkUploadDocs}";
  };

  uploadMainApps = {
    hackage.upload = {
      candidates = app "${uploadAll true false}";
      release = app "${uploadAll true true}";
      docs = app "${uploadAll false true}";
    };
  };

in {
  apps =
    util.mergeAllAttrs [
      uploadCandidates
      uploadReleases
      uploadDocs
      uploadMainApps
      mainApps
    ];
}
