{
dev = {
  exon = {
  meta = {
    sha256 = "07jawnnmpdqfnvmayv64xc4n0j9mbcgdyyqsg3dn3a3z1f4fxnfm";
    ver = "1.5.0.0";
  };
  drv = { mkDerivation, base, criterion, flatparse, generics-sop
, ghc-hs-meta, hedgehog, incipit-base, lib, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "exon";
  version = "1.5.0.0";
  src = /nix/store/jh1njmdacy2200l6ixaiirgr8lbnwj7v-source;
  libraryHaskellDepends = [
    base flatparse generics-sop ghc-hs-meta incipit-base
    template-haskell
  ];
  testHaskellDepends = [
    base hedgehog incipit-base tasty tasty-hedgehog template-haskell
  ];
  benchmarkHaskellDepends = [ base criterion incipit-base ];
  homepage = "https://github.com/tek/exon#readme";
  description = "Customizable quasiquote interpolation";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  flatparse = {
  meta = {
    sha256 = "0y6axksh2hqp8v58676a7zmwf0in7v6hmyfv8sfdx4x0acq2vjhr";
    ver = "0.5.0.1";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, containers, gauge
, hspec, HUnit, integer-gmp, lib, megaparsec, parsec, primitive
, QuickCheck, quickcheck-instances, template-haskell, utf8-string
}:
mkDerivation {
  pname = "flatparse";
  version = "0.5.0.1";
  src = /nix/store/xkg9z7kvk4a3v2dfl0mh3sz50wl56srx-source;
  libraryHaskellDepends = [
    base bytestring containers integer-gmp template-haskell utf8-string
  ];
  testHaskellDepends = [
    base bytestring hspec HUnit QuickCheck quickcheck-instances
    utf8-string
  ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring gauge integer-gmp megaparsec parsec
    primitive utf8-string
  ];
  homepage = "https://github.com/AndrasKovacs/flatparse#readme";
  description = "High-performance parsing from strict bytestrings";
  license = lib.licenses.mit;
}
;
}
;
};
ghc92 = {
  exon = {
  meta = {
    sha256 = "07jawnnmpdqfnvmayv64xc4n0j9mbcgdyyqsg3dn3a3z1f4fxnfm";
    ver = "1.5.0.0";
  };
  drv = { mkDerivation, base, criterion, flatparse, generics-sop
, ghc-hs-meta, hedgehog, incipit-base, lib, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "exon";
  version = "1.5.0.0";
  src = /nix/store/jh1njmdacy2200l6ixaiirgr8lbnwj7v-source;
  libraryHaskellDepends = [
    base flatparse generics-sop ghc-hs-meta incipit-base
    template-haskell
  ];
  testHaskellDepends = [
    base hedgehog incipit-base tasty tasty-hedgehog template-haskell
  ];
  benchmarkHaskellDepends = [ base criterion incipit-base ];
  homepage = "https://github.com/tek/exon#readme";
  description = "Customizable quasiquote interpolation";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  flatparse = {
  meta = {
    sha256 = "0y6axksh2hqp8v58676a7zmwf0in7v6hmyfv8sfdx4x0acq2vjhr";
    ver = "0.5.0.1";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, containers, gauge
, hspec, HUnit, integer-gmp, lib, megaparsec, parsec, primitive
, QuickCheck, quickcheck-instances, template-haskell, utf8-string
}:
mkDerivation {
  pname = "flatparse";
  version = "0.5.0.1";
  src = /nix/store/xkg9z7kvk4a3v2dfl0mh3sz50wl56srx-source;
  libraryHaskellDepends = [
    base bytestring containers integer-gmp template-haskell utf8-string
  ];
  testHaskellDepends = [
    base bytestring hspec HUnit QuickCheck quickcheck-instances
    utf8-string
  ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring gauge integer-gmp megaparsec parsec
    primitive utf8-string
  ];
  homepage = "https://github.com/AndrasKovacs/flatparse#readme";
  description = "High-performance parsing from strict bytestrings";
  license = lib.licenses.mit;
}
;
}
;
};
ghc94 = {
  exon = {
  meta = {
    sha256 = "07jawnnmpdqfnvmayv64xc4n0j9mbcgdyyqsg3dn3a3z1f4fxnfm";
    ver = "1.5.0.0";
  };
  drv = { mkDerivation, base, criterion, flatparse, generics-sop
, ghc-hs-meta, hedgehog, incipit-base, lib, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "exon";
  version = "1.5.0.0";
  src = /nix/store/jh1njmdacy2200l6ixaiirgr8lbnwj7v-source;
  libraryHaskellDepends = [
    base flatparse generics-sop ghc-hs-meta incipit-base
    template-haskell
  ];
  testHaskellDepends = [
    base hedgehog incipit-base tasty tasty-hedgehog template-haskell
  ];
  benchmarkHaskellDepends = [ base criterion incipit-base ];
  homepage = "https://github.com/tek/exon#readme";
  description = "Customizable quasiquote interpolation";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  flatparse = {
  meta = {
    sha256 = "0y6axksh2hqp8v58676a7zmwf0in7v6hmyfv8sfdx4x0acq2vjhr";
    ver = "0.5.0.1";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, containers, gauge
, hspec, HUnit, integer-gmp, lib, megaparsec, parsec, primitive
, QuickCheck, quickcheck-instances, template-haskell, utf8-string
}:
mkDerivation {
  pname = "flatparse";
  version = "0.5.0.1";
  src = /nix/store/xkg9z7kvk4a3v2dfl0mh3sz50wl56srx-source;
  libraryHaskellDepends = [
    base bytestring containers integer-gmp template-haskell utf8-string
  ];
  testHaskellDepends = [
    base bytestring hspec HUnit QuickCheck quickcheck-instances
    utf8-string
  ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring gauge integer-gmp megaparsec parsec
    primitive utf8-string
  ];
  homepage = "https://github.com/AndrasKovacs/flatparse#readme";
  description = "High-performance parsing from strict bytestrings";
  license = lib.licenses.mit;
}
;
}
;
};
hls = {
};
min = {
  exon = {
  meta = {
    sha256 = "07jawnnmpdqfnvmayv64xc4n0j9mbcgdyyqsg3dn3a3z1f4fxnfm";
    ver = "1.5.0.0";
  };
  drv = { mkDerivation, base, criterion, flatparse, generics-sop
, ghc-hs-meta, hedgehog, incipit-base, lib, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "exon";
  version = "1.5.0.0";
  src = /nix/store/jh1njmdacy2200l6ixaiirgr8lbnwj7v-source;
  libraryHaskellDepends = [
    base flatparse generics-sop ghc-hs-meta incipit-base
    template-haskell
  ];
  testHaskellDepends = [
    base hedgehog incipit-base tasty tasty-hedgehog template-haskell
  ];
  benchmarkHaskellDepends = [ base criterion incipit-base ];
  homepage = "https://github.com/tek/exon#readme";
  description = "Customizable quasiquote interpolation";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  flatparse = {
  meta = {
    sha256 = "0y6axksh2hqp8v58676a7zmwf0in7v6hmyfv8sfdx4x0acq2vjhr";
    ver = "0.5.0.1";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, containers, gauge
, hspec, HUnit, integer-gmp, lib, megaparsec, parsec, primitive
, QuickCheck, quickcheck-instances, template-haskell, utf8-string
}:
mkDerivation {
  pname = "flatparse";
  version = "0.5.0.1";
  src = /nix/store/xkg9z7kvk4a3v2dfl0mh3sz50wl56srx-source;
  libraryHaskellDepends = [
    base bytestring containers integer-gmp template-haskell utf8-string
  ];
  testHaskellDepends = [
    base bytestring hspec HUnit QuickCheck quickcheck-instances
    utf8-string
  ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring gauge integer-gmp megaparsec parsec
    primitive utf8-string
  ];
  homepage = "https://github.com/AndrasKovacs/flatparse#readme";
  description = "High-performance parsing from strict bytestrings";
  license = lib.licenses.mit;
}
;
}
;
};
}