{util}:
with builtins;
with util.lib;
let
  inherit (util) app build config project;

  pkgs = config.internal.pkgs;

  git = "${pkgs.git}/bin/git";

  cfg = config.hackage;

  gitAdd = cfg.commit || cfg.add;

  allTargets = if cfg.packages == null then config.internal.packageNames else cfg.packages;

  allCabals = concatMapStringsSep " " (n: "${project.packages.${n}.path}/${n}.cabal") allTargets;

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
      ${if needsGenCabal file then util.runBuildApp "gen-cabal-quiet" else ""}
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
    pkg = build.packages.dev.${target};
  in
    cfg.uploadCommand { inherit publish doc; path = tarball pkg; };

  sourceCommand =
    command false (pkg: "${pkg.release.sdist}/${pkg.package.pname}-${pkg.package.version}.tar.gz");

  docCommand =
    command true (pkg: "${pkg.release.haddock}/${pkg.package.pname}-${pkg.package.version}-docs.tar.gz");

  sourceCommands = publish:
  map (sourceCommand publish) allTargets;

  docCommands = publish:
  map (docCommand publish) allTargets;

  mkScript = name: script:
    util.zscript name ''
    setopt no_unset
    ${script}
    '';

  scriptBin = name: script:
    util.zscriptBin name ''
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

  appFor = doc: publish:
  if publish then (if doc then "release" else "release-source") else "candidates";

  packageAppFor = doc: publish:
  if publish then (if doc then "release" else "release-source") else "candidate";

  askAndUpload = doc: publish:
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
      ${util.runBuildApp "hackage.upload.${appFor doc publish}"} $version
    else
      ${util.runBuildApp ''hackage.bump.${packageAppFor doc publish}-''${pkg}''} $version
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

  uploadAll = source: doc: publish: let
    hookArgs = { inherit source publish; };
  in scriptBin "cabal-upload-all" (util.unlines (
    ["version=\${1:-}"] ++
    optionals source (sourceCommands publish) ++
    optionals doc (docCommands publish) ++
    [(cfg.hooks.postUploadAll hookArgs)] ++
    optional (source && publish && cfg.commit) commitFragment ++
    optional (source && publish && cfg.tag) tagFragment
  ));

  packageVersion = publish: doc: pkg:
  let
    file = config.packages.${pkg}.versionFile;
    type = packageAppFor doc publish;
  in scriptBin "cabal-upload-${pkg}" ''
  new_version=''${1:-}
  version=''${1:-}
  ${handleVersion file type}
  ${if publish && cfg.check then "nix flake check" else ""}
  ${util.runBuildApp "hackage.upload.${type}-${pkg}"} $version
  '';

  uploadPackage = source: doc: publish: name:
  scriptBin "cabal-upload-package" (util.unlines (
    ["version=\${1:-}"] ++
    optional source (sourceCommand publish name) ++
    optional doc (docCommand publish name) ++
    optional (source && publish && cfg.commit) (commitPackageFragment name) ++
    optional (source && publish && cfg.tag) (tagPackageFragment name)
  ));

  uploadPackageApps = source: doc: publish: type:
    let
      upload = uploadPackage source doc publish;
      version = packageVersion publish doc;
      target = n: {
        upload = { "${type}-${n}" = upload n; };
        bump = { "${type}-${n}" = version n; };
      };
    in { hackage = util.mergeAllAttrs (map target allTargets); };

  uploadCandidates =
    uploadPackageApps true true false "candidate";

  uploadReleases =
    uploadPackageApps true true true "release";

  uploadReleasesSource =
    uploadPackageApps true false true "release-source";

  uploadDocs =
    uploadPackageApps false true true "docs";

  mainApps = {
    candidates = app "${askAndUpload true false}";
    release = app "${askAndUpload true true}";
    release-source = app "${askAndUpload false true}";
    docs = app "${checkUploadDocs}";
  };

  uploadMainApps = {
    hackage.upload = {
      candidates = uploadAll true true false;
      release = uploadAll true true true;
      release-source = uploadAll true false true;
      docs = uploadAll false true true;
    };
  };

in {
  legacyPackages =
    util.mergeAllAttrs [
      uploadCandidates
      uploadReleases
      uploadReleasesSource
      uploadDocs
      uploadMainApps
    ];

  apps = mainApps;
}
