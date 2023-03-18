{ lib, config, util, ... }:
with builtins;
with lib;
let

  logic = import ../lib/hackage.nix { inherit lib config util; };

  isYaml = file: match ".*\.yaml" file != null;

  isNix = file: match ".*\.nix" file != null;

in {
  options.hackage = with types; {

    packages = mkOption {
      description = ''
        The set of packages that will be published to Hackage when the release command is run without arguments.
        If it is <literal>null</literal>, all packages are published.
        The items in the list should be Cabal package names as defined in <literal>options.packages</literal>.
      '';
      type = nullOr (listOf str);
      default = null;
    };

    allPackages = mkOption {
      description = ''
        There are two modes for versioning: Either all packages share the same version, in which case the release app
        will publish all packages at the same time, or each package has an individual version, in which case the release
        app expects the name of a package to be specified.
      '';
      type = bool;
      default = true;
    };

    versionFile = mkOption {
      description = ''
        If multiple packages use the same file for the version (like when using shared hpack files) this option may
        point to that file.
        If <literal>hackage.allPackages</literal> is <literal>true</literal> and this option is <literal>null</literal>,
        the version will not be modified by the release app.
        If the project uses the feature for hpack config synthesis from nix expressions, the version must be defined in
        a nix file. In that case, the simplest mechanism would be to use a separate file that only contains a string and
        is integrated into the config with <literal>version = import ./version.nix;</literal>. The default version
        handlers make this assumption; if a different method is used, the options
        <literal>hackage.versionFileExtract</literal> and <literal>hackage.versionFileUpdate</literal> must be adapted.
      '';
      type = nullOr str;
      default = null;
    };

    versionFiles = mkOption {
      description = ''
        Per-package version file paths.
        If <literal>hackage.allPackages</literal> is <literal>true</literal> and the specified package is not present in
        this set, the version will not be modified by the release app.
        See <literal>hackage.versionFile</literal> for more information.
      '';
      type = attrsOf str;
      default = {};
    };

    versionFileExtract = mkOption {
      description = ''
      A function that returns a shell script fragment that extracts the current version from a version file.
      The default assumes hpack/cabal format, like <literal>version: 5</literal>, unless the file has the extension
      <literal>.nix</literal>, in which case it is assumed the file only contains a string.
      '';
      type = functionTo str;
      default = file:
      if isNix file
      then ''sed -n 's/"\(.*\)"/\1/p' ${file}''
      else ''sed -n 's/^version:\s*\(\S\+\)/\1/p' ${file}'';
    };

    versionFileUpdate = mkOption {
      description = ''
      A function that returns a shell script fragment that updates the current version in a version file.
      The new version is stored in the environment variable <literal>$new_version</literal> in the surrounding shell
      script.
      The default assumes hpack/cabal format, like <literal>version: 5</literal>, unless the file has the extension
      <literal>.nix</literal>, in which case it is assumed the file only contains a string.
      '';
      type = functionTo str;
      default = file:
      if isNix file
      then ''sed -i "s/\".*\"/\"$new_version\"/" ${file}''
      else ''sed -i "s/^version:\(\s*\).*/version:\1$new_version/" ${file}'';
    };

    check = mkOption {
      description = ''
        Whether to run <literal>nix flake check</literal> before the release process.
      '';
      type = bool;
      default = true;
    };

    commit = mkOption {
      description = ''
        After successfully uploading a new release, the changes to the version file will be committed unless this is set
        to <literal>false</literal>.
      '';
      type = bool;
      default = true;
    };

    tag = mkOption {
      description = ''
        After successfully uploading a new release, a tag with the version name will be created unless this is set to
        <literal>false</literal>.
      '';
      type = bool;
      default = true;
    };

    uploadCommand = mkOption {
      description = ''
        The command used to upload a tarball, specified as a function that takes a set as a parameter with the
        attributes:
        <literal>
        {
          publish = "Boolean indicating whether this is a candidate or release";
          doc = "Boolean indicating whether this is a source or doc tarball";
          path = "The tarball's file path";
        }
        </literal>
      '';
      type = functionTo str;
      default = { publish, doc, path }:
      "cabal upload ${if publish then "--publish " else ""}${if doc then "--documentation " else ""}${path}";
    };

    askVersion = mkOption {
      description = "Whether to interactively query the user for a new version when releasing.";
      type = bool;
      default = true;
    };

    confirm = mkOption {
      description = "Whether to ask for confirmation before uploading.";
      type = bool;
      default = true;
    };

    output = {

      packages = mkOption {
        type = unspecified;
      };

      apps = mkOption {
        type = unspecified;
      };

    };

  };

  config.hackage.output = {
    packages = mkDefault logic.packages;

    apps = mkDefault logic.apps;
  };
}
