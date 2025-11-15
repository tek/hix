{util}: let

  inherit (util) lib;
  inherit (lib) types;

in {

  # TODO generalize with ghc-build
  options = {

    url = util.maybeOption types.str {
      description = "URL of a nixpkgs source archive or Git repo.";
    };

    rev = util.maybeOption types.str {
      description = ''
      Commit hash or ref name for the nixpkgs repo.
      If [](#opt-nixpkgs-source-url) is specified, this commit will be checked out from a cloned git repo.
      Otherwise, a tarball will be fetched from `nixos/nixpkgs`.
      '';
    };

    hash = lib.mkOption {
      description = "Hash of the nixpkgs source tree.";
      type = types.str;
      default = "";
    };

    args = lib.mkOption {
      description = ''
      Extra arguments for the fetcher (either `fetchurl` or `fetchgit` if [](#opt-nixpkgs-source-url) and
      [](#opt-nixpkgs-source-rev) are specified)
      '';
      type = types.attrsOf types.anything;
      default = {};
    };

  };

}
