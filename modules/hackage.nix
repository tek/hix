{lib, util, ...}:
{
  imports = import ./deprecation/hackage.nix { inherit lib; };

  options.hackage = {

    repos = lib.mkOption {
      description = ''
      Hackage repos used by the CLI for several tasks, like resolving managed dependencies and publishing packages and
      revisions.
      The default config consists of the usual server at `hackage.haskell.org`.
      '';
      type = lib.types.attrsOf (lib.types.submodule (import ./hackage-repo.nix { inherit util; }));
      default = {};
    };

  };

  config = {

    hackage.repos."hackage.haskell.org" = {
      description = lib.mkDefault "central Hackage";
      location = lib.mkDefault "https://hackage.haskell.org";
      solver = lib.mkDefault true;
      publish = lib.mkDefault true;
      secure = lib.mkDefault true;
    };

  };

}

