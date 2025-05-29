{lib, ...}: let

  cliReleaseOverride = false;

in {
  internal.removeFlakeLockFromCabalDrvSrc = true;
  internal.hixCli = lib.mkIf (!cliReleaseOverride) {
    commit = "7fcff73ff9844321b05c8cd567a9c1c76c6aa7f9";
    sha256 = "1rzdba3ngqaf1gvc22nljpmkvi5qh61c8hld4a0y9f0x9wgg4drs";
  };
}
