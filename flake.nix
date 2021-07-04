{
  description = "Haskell Development Build Tools";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/432fc2d9a67f92e05438dff5fdc2b39d33f77997;
    nixpkgs901.url = github:NixOS/nixpkgs/432fc2d9a67f92e05438dff5fdc2b39d33f77997;
    nixpkgs8104.url = github:NixOS/nixpkgs/842f900e73c7ce985218cc4f455e34d1d56475c1;
    nixpkgs8105.url = github:NixOS/nixpkgs/432fc2d9a67f92e05438dff5fdc2b39d33f77997;
    nixpkgs884.url = github:NixOS/nixpkgs/c0e881852006b132236cbf0301bd1939bb50867e;
    nixpkgs865.url = github:NixOS/nixpkgs/cfed29bfcb28259376713005d176a6f82951014a;
    easy-hls.url = github:jkachmar/easy-hls-nix;
    flake-utils.url = github:numtide/flake-utils;
    obelisk = {
      url = github:tek/obelisk/tryp;
      flake = false;
    };
    snap-core = {
      url = github:obsidiansystems/snap-core?ref=ts-expose-fileserve-internals;
      flake = false;
    };
    thax.url = github:tek/thax;
    blaze-textual = {
      url = github:jwaldmann/blaze-textual;
      flake = false;
    };
    hie-bios = {
      url = github:jneira/hie-bios/9b1445ab5efcabfad54043fc9b8e50e9d8c5bbf3;
      flake = false;
    };
    ghc-api-compat = {
      url = github:hsyl20/ghc-api-compat/8fee87eac97a538dbe81ff1ab18cff10f2f9fa15;
      flake = false;
    };
    th-extras = {
      url = github:anka-213/th-extras/57a97b4df128eb7b360e8ab9c5759392de8d1659;
      flake = false;
    };
    dependent-sum = {
      url = github:anka-213/dependent-sum/8cf4c7fbc3bfa2be475a17bb7c94a1e1e9a830b5;
      flake = false;
    };
    lsp = {
      url = github:anka-213/lsp/tag-ghc-9.0.1-without-pr-326;
      flake = false;
    };
    czipwith = {
      url = github:mithrandi/czipwith/b6245884ae83e00dd2b5261762549b37390179f8;
      flake = false;
    };
    hls = {
      url = github:haskell/haskell-language-server/b6245884ae83e00dd2b5261762549b37390179f8;
      flake = false;
    };
  };

  outputs = inputs: import ./main.nix inputs;
}
