{util}: let

  inherit (util) config lib;

  cli = config.internal.hixCli.exe;

  releaseConfig = config.release;

  # Build CLI arguments from release config options
  cliArgs = lib.concatStringsSep " " (lib.filter (s: s != "") [
    (lib.optionalString releaseConfig.check "--check")
    (lib.optionalString releaseConfig.commit "--commit")
    (lib.optionalString releaseConfig.tag "--tag")
    (lib.optionalString (!releaseConfig.hermetic) "--global-cabal-config")
    (lib.optionalString releaseConfig.interactive "--interactive")
    (packagesArgs releaseConfig.packages)
  ]);

  packagesArgs = packages:
    if packages == null
    then ""
    else lib.concatMapStringsSep " " (p: "--package ${p}") packages;

  app = suffix: args:
    let allArgs = lib.concatStringsSep " " (lib.filter (s: s != "") [cliArgs args]);
    in util.bapp "hix-release${suffix}" ''
    ${cli} hackage release ${allArgs} $@
    '';

  apps = {
    release = app "" "";
    candidates = app "-candidates" "--candidates";
    release-source = app "-source" "--publish=sources";
    docs = app "-docs" "--publish=docs";
  };

in {
  inherit apps;
}
