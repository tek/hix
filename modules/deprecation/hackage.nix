{lib}: let

  boot = import ../../lib/lib/boot.nix { inherit lib; };

  opt = args: boot.deprecatedOption (args // { path = ["hackage"] ++ args.path; });

in map opt [
  { path = ["packages"]; replacement = "release.packages"; }
  { path = ["allPackages"];
    replacement = "--package <name>-<version>";
    extra = "Use --package <name>-<version> to specify explicit versions, or --interactive for UI-based version selection.";
  }
  { path = ["versionFile"]; replacement = "release.versionFile"; }
  { path = ["setChangelogVersion"]; replacement = "release.setChangelogVersion"; }
  { path = ["commit"]; replacement = "release.commit"; }
  { path = ["commitExtraArgs"]; replacement = "release.commitExtraArgs"; }
  { path = ["add"];
    replacement = "release.hooks";
    extra = "Use a pre-commit hook in release.hooks with commit = false to achieve the same effect.";
  }
  { path = ["tag"]; replacement = "release.tag"; }
  { path = ["tagExtraArgs"]; replacement = "release.tagExtraArgs"; }
  { path = ["formatTag"];
    replacement = "release.hooks";
    extra = "Disable tagging with tag = false and create custom tags in a post-upload hook.";
  }
  { path = ["uploadCommand"];
    extra = "The release CLI now handles uploads internally using the Hackage repos configuration.";
  }
  { path = ["cabalArgs"];
    replacement = "hermetic = false";
    extra = "Set hermetic = false and configure Cabal via ~/.cabal/config instead.";
  }
  { path = ["cabalUploadArgs"];
    replacement = "hermetic = false";
    extra = "Set hermetic = false and configure Cabal via ~/.cabal/config instead.";
  }
  { path = ["askVersion"];
    replacement = "release.interactive";
    extra = "Use the interactive option to enable UI-based version selection.";
  }
  { path = ["confirm"];
    replacement = "release.interactive";
    extra = "Use the interactive option to enable UI-based confirmation.";
  }
  { path = ["hooks" "postUploadAll"];
    replacement = "release.hooks";
    extra = "Use release.hooks with executable files that receive context via environment variables.";
  }
  { path = ["hooks" "preCommitAll"];
    replacement = "release.hooks";
    extra = "Use release.hooks with executable files that receive context via environment variables.";
  }
  { path = ["hooks" "postCommitAll"];
    replacement = "release.hooks";
    extra = "Use release.hooks with executable files that receive context via environment variables.";
  }
]
