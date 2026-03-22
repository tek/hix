{
  description = "Test: transitive compiler extension with partial override";
  inputs.hix.url = "HIX";
  outputs = {self, hix}: hix.lib._hix_test ({config, ...}: {
    inherit self;
    packages.root.src = ./.;

    outputs.legacyPackages = {
      leaf-source = config.compilers.leaf.source;
      mid-tag = config.compilers.mid.tag;
      leaf-tag = config.compilers.leaf.tag;
      leaf-name = config.compilers.leaf.name;
      # Check that build config propagates transitively
      leaf-flavour = config.compilers.leaf.source.build.flavour;
      mid-flavour = config.compilers.mid.source.build.flavour;
    };

    compilers.base = {
      extends = null;
      source.build = {
        version = "9.10.1";
        hash = "sha256-vzhqMC1O4FR5H/1RdIkA8V1xdg/RmRV5ItEgzB+J4vc=";
        flavour = "quickest";
      };
      tag = "base-tag";
    };

    # mid extends base, overrides tag, inherits source.build
    compilers.mid = {
      extends = "base";
      tag = "mid-tag";
    };

    # leaf extends mid, overrides flavour, should inherit everything else
    compilers.leaf = {
      extends = "mid";
      source.build.flavour = "quick";
    };
  });
}
