{lib, ...}: let

  cliReleaseOverride = false;

in {
  internal.removeFlakeLockFromCabalDrvSrc = true;
  internal.hixCli = lib.mkIf (!cliReleaseOverride) {
    dev = true;
    # commit = "8d92716141f252642a45b0ff0d5468e28a0701c7";
    # sha256 = "05wp05hdls051xjavnkzppk494zigr9bp6pqhhlbydkrfc455fbz";
  };
}
