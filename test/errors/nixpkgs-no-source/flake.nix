{
  description = "Test: nixpkgs config without url or rev";
  inputs.hix.url = "HIX";
  outputs = {self, hix}: hix.lib._hix_test {
    inherit self;
    packages.root.src = ./.;
    nixpkgs.bad = { extends = null; source = {}; };
    compilers.bad = {
      source = "ghc910";
      nixpkgs = "bad";
    };
    envs.bad.compiler = "bad";
  };
}
