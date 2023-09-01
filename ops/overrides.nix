{
dev = {
  exon = {
  meta = {
    sha256 = "1m4i3a14wip985ncblfy2ikcy7gw5rryj9z497ah218d1nmwj7rl";
    ver = "1.4.0.0";
  };
  drv = { mkDerivation, base, criterion, flatparse, generics-sop
, ghc-hs-meta, hedgehog, incipit-base, lib, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "exon";
  version = "1.4.0.0";
  src = /nix/store/rrbiqj1v72nbkwd2nqkd303sczq1h63y-source;
  libraryHaskellDepends = [
    base flatparse generics-sop ghc-hs-meta incipit-base
    template-haskell
  ];
  testHaskellDepends = [
    base hedgehog incipit-base tasty tasty-hedgehog template-haskell
  ];
  benchmarkHaskellDepends = [ base criterion incipit-base ];
  homepage = "https://git.tryp.io/tek/exon";
  description = "Customizable Quasiquote Interpolation";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  flatparse = {
  meta = {
    sha256 = "0saxwgwbzijgm9v5w9nx3npl28szpkyz97m4shn8yanxq7gsjnvg";
    ver = "0.4.0.2";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, containers, gauge
, hspec, HUnit, integer-gmp, lib, megaparsec, parsec, primitive
, QuickCheck, quickcheck-instances, template-haskell, utf8-string
}:
mkDerivation {
  pname = "flatparse";
  version = "0.4.0.2";
  src = /nix/store/iycca9g4v7p08zkxka42402fawmpyrd7-source;
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
    sha256 = "1m4i3a14wip985ncblfy2ikcy7gw5rryj9z497ah218d1nmwj7rl";
    ver = "1.4.0.0";
  };
  drv = { mkDerivation, base, criterion, flatparse, generics-sop
, ghc-hs-meta, hedgehog, incipit-base, lib, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "exon";
  version = "1.4.0.0";
  src = /nix/store/rrbiqj1v72nbkwd2nqkd303sczq1h63y-source;
  libraryHaskellDepends = [
    base flatparse generics-sop ghc-hs-meta incipit-base
    template-haskell
  ];
  testHaskellDepends = [
    base hedgehog incipit-base tasty tasty-hedgehog template-haskell
  ];
  benchmarkHaskellDepends = [ base criterion incipit-base ];
  homepage = "https://git.tryp.io/tek/exon";
  description = "Customizable Quasiquote Interpolation";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  flatparse = {
  meta = {
    sha256 = "0saxwgwbzijgm9v5w9nx3npl28szpkyz97m4shn8yanxq7gsjnvg";
    ver = "0.4.0.2";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, containers, gauge
, hspec, HUnit, integer-gmp, lib, megaparsec, parsec, primitive
, QuickCheck, quickcheck-instances, template-haskell, utf8-string
}:
mkDerivation {
  pname = "flatparse";
  version = "0.4.0.2";
  src = /nix/store/iycca9g4v7p08zkxka42402fawmpyrd7-source;
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
    sha256 = "1m4i3a14wip985ncblfy2ikcy7gw5rryj9z497ah218d1nmwj7rl";
    ver = "1.4.0.0";
  };
  drv = { mkDerivation, base, criterion, flatparse, generics-sop
, ghc-hs-meta, hedgehog, incipit-base, lib, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "exon";
  version = "1.4.0.0";
  src = /nix/store/rrbiqj1v72nbkwd2nqkd303sczq1h63y-source;
  libraryHaskellDepends = [
    base flatparse generics-sop ghc-hs-meta incipit-base
    template-haskell
  ];
  testHaskellDepends = [
    base hedgehog incipit-base tasty tasty-hedgehog template-haskell
  ];
  benchmarkHaskellDepends = [ base criterion incipit-base ];
  homepage = "https://git.tryp.io/tek/exon";
  description = "Customizable Quasiquote Interpolation";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  flatparse = {
  meta = {
    sha256 = "0saxwgwbzijgm9v5w9nx3npl28szpkyz97m4shn8yanxq7gsjnvg";
    ver = "0.4.0.2";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, containers, gauge
, hspec, HUnit, integer-gmp, lib, megaparsec, parsec, primitive
, QuickCheck, quickcheck-instances, template-haskell, utf8-string
}:
mkDerivation {
  pname = "flatparse";
  version = "0.4.0.2";
  src = /nix/store/iycca9g4v7p08zkxka42402fawmpyrd7-source;
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
    sha256 = "1m4i3a14wip985ncblfy2ikcy7gw5rryj9z497ah218d1nmwj7rl";
    ver = "1.4.0.0";
  };
  drv = { mkDerivation, base, criterion, flatparse, generics-sop
, ghc-hs-meta, hedgehog, incipit-base, lib, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "exon";
  version = "1.4.0.0";
  src = /nix/store/rrbiqj1v72nbkwd2nqkd303sczq1h63y-source;
  libraryHaskellDepends = [
    base flatparse generics-sop ghc-hs-meta incipit-base
    template-haskell
  ];
  testHaskellDepends = [
    base hedgehog incipit-base tasty tasty-hedgehog template-haskell
  ];
  benchmarkHaskellDepends = [ base criterion incipit-base ];
  homepage = "https://git.tryp.io/tek/exon";
  description = "Customizable Quasiquote Interpolation";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  flatparse = {
  meta = {
    sha256 = "0saxwgwbzijgm9v5w9nx3npl28szpkyz97m4shn8yanxq7gsjnvg";
    ver = "0.4.0.2";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, containers, gauge
, hspec, HUnit, integer-gmp, lib, megaparsec, parsec, primitive
, QuickCheck, quickcheck-instances, template-haskell, utf8-string
}:
mkDerivation {
  pname = "flatparse";
  version = "0.4.0.2";
  src = /nix/store/iycca9g4v7p08zkxka42402fawmpyrd7-source;
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