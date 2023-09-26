{ lib, config, util, ... }:
with builtins;
with lib;
let

  logic = import ../lib/hackage.nix { inherit lib config util; };

  isNix = file: match ".*\.nix" file != null;

in {
  options.hackage = with types; {

    packages = mkOption {
      description = mdDoc ''
        The set of packages that will be published to Hackage when the release command is run without arguments.
        If it is `null`, all packages are published.
        The items in the list should be Cabal package names as defined in `options.packages`.
      '';
      type = nullOr (listOf str);
      default = null;
    };

    allPackages = mkOption {
      description = mdDoc ''
        There are two modes for versioning: Either all packages share the same version, in which case the release app
        will publish all packages at the same time, or each package has an individual version, in which case the release
        app expects the name of a package to be specified.
      '';
      type = bool;
      default = true;
    };

    versionFile = mkOption {
      description = mdDoc ''
        If multiple packages use the same file for the version (like when using shared hpack files) this option may
        point to that file.
        If `hackage.allPackages` is `true` and this option is `null`, the version will not be modified by the release
        app.
        If the project uses the feature for hpack config synthesis from nix expressions, the version must be defined in
        a nix file.
        In that case, the simplest mechanism would be to use a separate file that only contains a string and is
        integrated into the config with `version = import ./version.nix;`.
        The default version handlers make this assumption; if a different method is used, the options
        `hackage.versionFileExtract` and `hackage.versionFileUpdate` must be adapted.
      '';
      type = nullOr str;
      default = null;
    };

    versionFileExtract = mkOption {
      description = mdDoc ''
      A function that returns a shell script fragment that extracts the current version from a version file.
      The default assumes hpack/cabal format, like `version: 5`, unless the file has the extension
      `.nix`, in which case it is assumed the file only contains a string.
      '';
      type = functionTo str;
      default = file:
      if isNix file
      then ''sed -n 's/"\(.*\)"/\1/p' ${file}''
      else ''sed -n 's/^version:\s*\(\S\+\)/\1/p' ${file}'';
    };

    versionFileUpdate = mkOption {
      description = mdDoc ''
      A function that returns a shell script fragment that updates the current version in a version file.
      The new version is stored in the environment variable `$new_version` in the surrounding shell
      script.
      The default assumes hpack/cabal format, like `version: 5`, unless the file has the extension
      `.nix`, in which case it is assumed the file only contains a string.
      '';
      type = functionTo str;
      default = file:
      if isNix file
      then ''sed -i "s/\".*\"/\"$new_version\"/" ${file}''
      else ''sed -i "s/^version:\(\s*\).*/version:\1$new_version/" ${file}'';
    };

    setChangelogVersion = mkOption {
      description = mdDoc "Whether to substitute the word 'Unreleased' with the new version in changelogs.";
      type = bool;
      default = false;
    };

    check = mkOption {
      description = mdDoc ''
        Whether to run `nix flake check` before the release process.
      '';
      type = bool;
      default = true;
    };

    commit = mkOption {
      description = mdDoc ''
        After successfully uploading a new release, the changes to the version file, cabal files and changelog will be
        committed unless this is set to `false`.
      '';
      type = bool;
      default = true;
    };

    add = mkOption {
      description = mdDoc ''
      When [](#opt-hackage-hackage.add) is set to `false`, this option can be enabled to git-add the files but not
      commit them.
      '';
      type = bool;
      default = false;
    };

    tag = mkOption {
      description = mdDoc ''
        After successfully uploading a new release, a tag with the version name will be created unless this is set to
        `false`.
      '';
      type = bool;
      default = true;
    };

    formatTag = mkOption {
      description = mdDoc "Function that creates a tag name from a version and an optional package name.";
      type = functionTo str;
      default = { name, version }: let
        vtag = "v${version}";
      in if name == null then vtag else "${name}-${vtag}";
    };

    uploadCommand = mkOption {
      description = mdDoc ''
        The command used to upload a tarball, specified as a function that takes a set as a parameter with the
        attributes:
        ```
        {
          publish = "Boolean indicating whether this is a candidate or release";
          doc = "Boolean indicating whether this is a source or doc tarball";
          path = "The tarball's file path";
        }
        ```
      '';
      type = functionTo str;
      default = { publish, doc, path }:
      "cabal upload ${if publish then "--publish " else ""}${if doc then "--documentation " else ""}${path}";
    };

    askVersion = mkOption {
      description = mdDoc "Whether to interactively query the user for a new version when releasing.";
      type = bool;
      default = true;
    };

    confirm = mkOption {
      description = mdDoc "Whether to ask for confirmation before uploading.";
      type = bool;
      default = true;
    };

    output = {

      packages = mkOption {
        description = mdDoc "Internal option";
        type = unspecified;
      };

      apps = mkOption {
        description = mdDoc "Internal option";
        type = unspecified;
      };

    };

  };

  config.hackage.output = {
    packages = mkDefault logic.packages;

    apps = mkDefault logic.apps;
  };
}
