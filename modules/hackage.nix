{lib, util, ...}:
let
  inherit (util) internal;
  inherit (lib) types;

  deprecatedOption = internal.modules.deprecatedOption;

  repoModule = import ./hackage-repo.nix { inherit util; };

in {
  options.hackage = {

    packages = deprecatedOption {
      type = types.nullOr (types.listOf util.types.localPackage);
      key = "hackage.packages";
      replacement = "release.packages";
      description = "Deprecated. Use release.packages instead.";
    };

    repos = lib.mkOption {
      description = ''
      Hackage repos used by the CLI for several tasks, like resolving managed dependencies and publishing packages and
      revisions.
      The default config consists of the usual server at `hackage.haskell.org`.
      '';
      type = types.attrsOf (types.submodule repoModule);
      default = {};
    };

    allPackages = deprecatedOption {
      type = types.bool;
      key = "hackage.allPackages";
      replacement = "--package <name>-<version>";
      extra = "Use --package <name>-<version> to specify explicit versions, or --interactive for UI-based version selection.";
      description = "Deprecated. Use --package or --interactive instead.";
    };

    versionFile = deprecatedOption {
      type = types.nullOr types.str;
      key = "hackage.versionFile";
      replacement = "release.versionFile";
      description = "Deprecated. Use release.versionFile instead.";
    };

    setChangelogVersion = deprecatedOption {
      type = types.bool;
      key = "hackage.setChangelogVersion";
      replacement = "release.setChangelogVersion";
      description = "Deprecated. Use release.setChangelogVersion instead.";
    };

    commit = deprecatedOption {
      type = types.bool;
      key = "hackage.commit";
      replacement = "release.commit";
      description = "Deprecated. Use release.commit instead.";
    };

    commitExtraArgs = deprecatedOption {
      type = types.listOf types.str;
      key = "hackage.commitExtraArgs";
      replacement = "release.commitExtraArgs";
      description = "Deprecated. Use release.commitExtraArgs instead.";
    };

    add = deprecatedOption {
      type = types.bool;
      key = "hackage.add";
      replacement = "release.hooks";
      extra = "Use a pre-commit hook in release.hooks with commit = false to achieve the same effect.";
      description = "Deprecated. Use release.hooks instead.";
    };

    tag = deprecatedOption {
      type = types.bool;
      key = "hackage.tag";
      replacement = "release.tag";
      description = "Deprecated. Use release.tag instead.";
    };

    tagExtraArgs = deprecatedOption {
      type = types.listOf types.str;
      key = "hackage.tagExtraArgs";
      replacement = "release.tagExtraArgs";
      description = "Deprecated. Use release.tagExtraArgs instead.";
    };

    formatTag = deprecatedOption {
      type = types.functionTo types.str;
      key = "hackage.formatTag";
      replacement = "release.hooks";
      extra = "Disable tagging with tag = false and create custom tags in a post-upload hook.";
      description = "Deprecated. Use release.hooks instead.";
    };

    uploadCommand = deprecatedOption {
      type = types.functionTo types.str;
      key = "hackage.uploadCommand";
      extra = "The release CLI now handles uploads internally using the Hackage repos configuration.";
      description = "Deprecated. Uploads are handled internally by the release CLI.";
    };

    cabalArgs = deprecatedOption {
      type = types.str;
      key = "hackage.cabalArgs";
      replacement = "hermetic = false";
      extra = "Set hermetic = false and configure Cabal via ~/.cabal/config instead.";
      description = "Deprecated. Use hermetic = false and configure Cabal manually.";
    };

    cabalUploadArgs = deprecatedOption {
      type = types.str;
      key = "hackage.cabalUploadArgs";
      replacement = "hermetic = false";
      extra = "Set hermetic = false and configure Cabal via ~/.cabal/config instead.";
      description = "Deprecated. Use hermetic = false and configure Cabal manually.";
    };

    askVersion = deprecatedOption {
      type = types.bool;
      key = "hackage.askVersion";
      replacement = "release.interactive";
      extra = "Use the interactive option to enable UI-based version selection.";
      description = "Deprecated. Use release.interactive instead.";
    };

    confirm = deprecatedOption {
      type = types.bool;
      key = "hackage.confirm";
      replacement = "release.interactive";
      extra = "Use the interactive option to enable UI-based confirmation.";
      description = "Deprecated. Use release.interactive instead.";
    };

    hooks = {

      postUploadAll = deprecatedOption {
        type = types.functionTo types.lines;
        key = "hackage.hooks";
        replacement = "release.hooks";
        extra = "Use release.hooks with executable files that receive context via environment variables.";
        description = "Deprecated. Use release.hooks instead.";
      };

      preCommitAll = deprecatedOption {
        type = types.lines;
        key = "hackage.hooks";
        replacement = "release.hooks";
        extra = "Use release.hooks with executable files that receive context via environment variables.";
        description = "Deprecated. Use release.hooks instead.";
      };

      postCommitAll = deprecatedOption {
        type = types.lines;
        key = "hackage.hooks";
        replacement = "release.hooks";
        extra = "Use release.hooks with executable files that receive context via environment variables.";
        description = "Deprecated. Use release.hooks instead.";
      };

    };

  };

  config.hackage = {

    repos."hackage.haskell.org" = {
      description = lib.mkDefault "central Hackage";
      location = lib.mkDefault "https://hackage.haskell.org";
      solver = lib.mkDefault true;
      publish = lib.mkDefault true;
      secure = lib.mkDefault true;
    };

  };

}
