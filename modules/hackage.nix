{ lib, config, unlines, foldMapAttrs, ... }:
with lib;
let
  logic = import ../lib/hackage.nix { inherit lib config unlines foldMapAttrs; };
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
        If <literal>releaseAll</literal> is <literal>true</literal> and this option is <literal>null</literal>, the
        version will not be modified by the release app.
      '';
      type = nullOr str;
      default = null;
    };

    versionFiles = mkOption {
      description = ''
        Per-package version file paths.
        If <literal>releaseAll</literal> is <literal>true</literal> and the specified package is not present in this
        set, the version will not be modified by the release app.
      '';
      type = attrsOf str;
      default = {};
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
