{ config, lib, util, ... }:
let
  inherit (lib) types;

  logic = import ../lib/hackage.nix { inherit util; };

  isNix = file: builtins.match ".*\.nix" file != null;

in {
  options.hackage = {

    packages = lib.mkOption {
      description = ''
        The set of packages that will be published to Hackage when the release command is run without arguments.
        If it is `null`, all packages are published.
        The items in the list should be Cabal package names as defined in `options.packages`.
      '';
      type = types.nullOr (types.listOf types.str);
      default = null;
    };

    allPackages = lib.mkOption {
      description = ''
        There are two modes for versioning: Either all packages share the same version, in which case the release app
        will publish all packages at the same time, or each package has an individual version, in which case the release
        app expects the name of a package to be specified.
      '';
      type = types.bool;
      default = true;
    };

    versionFile = lib.mkOption {
      description = ''
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
      type = types.nullOr types.str;
      default = null;
    };

    versionFileExtract = lib.mkOption {
      description = ''
      A function that returns a shell script fragment that extracts the current version from a version file.
      The default assumes hpack/cabal format, like `version: 5`, unless the file has the extension
      `.nix`, in which case it is assumed the file only contains a string.
      '';
      type = types.functionTo types.str;
      default = file:
      if isNix file
      then ''sed -n 's/"\(.*\)"/\1/p' ${file}''
      else ''sed -n 's/^version:\s*\(\S\+\)/\1/p' ${file}'';
    };

    versionFileUpdate = lib.mkOption {
      description = ''
      A function that returns a shell script fragment that updates the current version in a version file.
      The new version is stored in the environment variable `$new_version` in the surrounding shell
      script.
      The default assumes hpack/cabal format, like `version: 5`, unless the file has the extension
      `.nix`, in which case it is assumed the file only contains a string.
      '';
      type = types.functionTo types.str;
      default = file:
      if isNix file
      then ''sed -i "s/\".*\"/\"$new_version\"/" ${file}''
      else ''sed -i "s/^version:\(\s*\).*/version:\1$new_version/" ${file}'';
    };

    setChangelogVersion = lib.mkOption {
      description = "Whether to substitute the word 'Unreleased' with the new version in changelogs.";
      type = types.bool;
      default = false;
    };

    check = lib.mkOption {
      description = ''
        Whether to run `nix flake check` before the release process.
      '';
      type = types.bool;
      default = true;
    };

    commit = lib.mkOption {
      description = ''
        After successfully uploading a new release, the changes to the version file, cabal files and changelog will be
        committed unless this is set to `false`.
      '';
      type = types.bool;
      default = true;
    };

    commitExtraArgs = lib.mkOption {
      description = "Extra CLI options for `git commit`.";
      type = types.str;
      default = "";
    };

    add = lib.mkOption {
      description = ''
      When [](#opt-hackage-hackage.add) is set to `false`, this option can be enabled to git-add the files but not
      commit them.
      '';
      type = types.bool;
      default = false;
    };

    tag = lib.mkOption {
      description = ''
        After successfully uploading a new release, a tag with the version name will be created unless this is set to
        `false`.
      '';
      type = types.bool;
      default = true;
    };

    tagExtraArgs = lib.mkOption {
      description = "Extra CLI options for `git tag`.";
      type = types.str;
      default = "";
    };

    formatTag = lib.mkOption {
      description = "Function that creates a tag name from a version and an optional package name.";
      type = types.functionTo types.str;
      default = { name, version }:
      if name == null then version else "${name}-${version}";
    };

    uploadCommand = lib.mkOption {
      description = ''
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
      default = { publish, doc, path }:
      "cabal upload ${if publish then "--publish " else ""}${if doc then "--documentation " else ""}${path}";
      type = types.functionTo types.str;
    };

    askVersion = lib.mkOption {
      description = "Whether to interactively query the user for a new version when releasing.";
      type = types.bool;
      default = true;
    };

    confirm = lib.mkOption {
      description = "Whether to ask for confirmation before uploading.";
      type = types.bool;
      default = true;
    };

    hooks = {

      postUploadAll = lib.mkOption {
        description = ''
        Shell script lines (zsh) to run after uploading all packages.

        Value is a function that gets the set `{source, publish}`, two booleans that indicate whether the sources (or
        only docs) were uploaded, and whether the artifacts were published (or just candidates).
        '';
        type = types.functionTo types.lines;
        default = _: "";
      };

      preCommitAll = lib.mkOption {
        description = ''
        Shell script lines (zsh) to run before commiting the version change after publishing all packages.
        '';
        type = types.lines;
        default = "";
      };

      postCommitAll = lib.mkOption {
        description = ''
        Shell script lines (zsh) to run after commiting the version change after publishing all packages.
        '';
        type = types.lines;
        default = "";
      };

    };

    output = {

      packages = lib.mkOption {
        description = "Internal option";
        type = types.unspecified;
      };

      apps = lib.mkOption {
        description = "Internal option";
        type = types.unspecified;
      };

    };

  };

  config.hackage.output = {
    packages = mkDefault logic.packages;

    apps = mkDefault logic.apps;
  };
}
