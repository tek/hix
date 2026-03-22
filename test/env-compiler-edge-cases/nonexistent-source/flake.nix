{
  description = "Test: env compiler shortcut with truly nonexistent compiler source";
  inputs.hix.url = "HIX";
  outputs = {self, hix}: hix.lib._hix_test {
    inherit self;
    packages.root.src = ./.;
    # "ghcXYZ" doesn't exist in compilers.* or nixpkgs haskell.packages.
    envs.bad.compiler = "ghcXYZ";
  };
}
