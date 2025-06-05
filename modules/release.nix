{config, lib, util, ...}:
let
  inherit (util) internal;
  inherit (lib) types;

in {

  options.release = {

    packages = util.maybeOption (types.listOf util.types.localPackage) {
      description = ''
        The set of packages that will be published to Hackage when the release command is run.
        If it is `null`, all packages are published, but you can still select them using `--package foo`.
        The items in the list should be Cabal package names as defined in [](#opt-general-packages).
      '';
    };

    versionFile = lib.mkOption {
      description = ''
        Path to a version file that should be updated during the release process.
        If this is set, all packages share the same version, unless they also set [](#opt-package-versionFile).

        This should be a `.nix` files and contain only a quoted string, e.g., `"1.2.3"`.

        The default definition for the option [](#opt-cabal-version) reads this file.
      '';
      type = types.nullOr types.str;
      default = null;
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
        After successfully uploading a new release, the changes to the repository will be committed if this is set.
      '';
      type = types.bool;
      default = false;
    };

    commitExtraArgs = lib.mkOption {
      description = "Extra CLI options for `git commit`.";
      type = types.listOf types.str;
      default = [];
    };

    tag = lib.mkOption {
      description = ''
        After successfully uploading a new release, a tag with the version name will be created if this is set.
      '';
      type = types.bool;
      default = false;
    };

    tagExtraArgs = lib.mkOption {
      description = "Extra CLI options for `git tag`.";
      type = types.listOf types.str;
      default = [];
    };

    hermetic = lib.mkOption {
      description = "Whether to ignore the Cabal config in `$HOME`.";
      type = types.bool;
      default = true;
    };

    interactive = lib.mkOption {
      description = ''
        Whether to use the interactive UI for version selection and upload confirmation.
        When disabled, versions must be specified via --version or --package flags.
      '';
      type = types.bool;
      default = false;
    };

    hooks = lib.mkOption {
      description = ''
      An list of executable files that are invoked at certain points during the release process, allowing custom steps
      to be performed.
      The process environment contains information about the state of the release process in these variables:
      - `context` is a JSON value with these fields:
        - `phase` describes the associated release step, either `post-upload` or `post-commit`
        - `packages` is a list of objects describing the state of each package, with these fields:
          - `package`: Package name
          - `version`: Released version
          - `success`: Boolean indicating whether uploading succeeded
        - `publish` and `candidates` indicate which artifacts have been uploaded for the two categories, both consisting
          of an object with the keys `sources` and `docs` mapping to booleans
      - `version` is a plain string containing the release version, but only if all released packages have the same
        version.

      May also be specified as an inline bash script, as a string without shebang.
      '';
      type = types.listOf (types.either types.lines types.path);
      default = [];
    };

  };

  config.release = {

    hooks = lib.optional config.release.setChangelogVersion internal.release.hookUpdateChangelogs;

  };

}
