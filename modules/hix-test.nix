{lib, ...}: let

  cliReleaseOverride = false;

in {
  internal.removeFlakeLockFromCabalDrvSrc = true;
  internal.hixCli = lib.mkIf (!cliReleaseOverride) {
    dev = true;
    # commit = "6a5f1e23c7d25343d01728b3e1666c200b833183";
    # sha256 = "15rd5q8vx61zjfbm040dbgqd345abaqkwbwb9npjx496ggq1bvlf";
  };
}
