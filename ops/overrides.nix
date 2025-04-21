{
dev = {
};
ghc96 = {
};
ghc98 = {
};
hix-build-tools = {
};
hls = {
};
integration = {
};
latest = {
  exon = {
  meta = {
    sha256 = "0hg271cvjqm4ps75qpnirq9nvjwpwb03mcbn1a364jrysrj6bg3b";
    url = "https://hackage.haskell.org";
    ver = "1.7.2.0";
  };
  drv = { mkDerivation, base, criterion, ghc, hedgehog, incipit-base, lib
, parsec, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "exon";
  version = "1.7.2.0";
  src = /nix/store/scamv6qgdfzmlicp6wsk76vg2ls6kznd-source;
  libraryHaskellDepends = [
    base ghc incipit-base parsec template-haskell
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
  extra = {
  meta = {
    sha256 = "0cnk9ncn0k7fv24g0v3rhqd3z9zcz9cgz0rf59vs6v9kappbidmx";
    url = "https://hackage.haskell.org";
    ver = "1.8";
  };
  drv = { mkDerivation, base, clock, directory, filepath, lib, process
, QuickCheck, quickcheck-instances, time, unix
}:
mkDerivation {
  pname = "extra";
  version = "1.8";
  src = /nix/store/ypql7lr4d6drwzkr56lpdq5qspd9bc8b-source;
  libraryHaskellDepends = [
    base clock directory filepath process time unix
  ];
  testHaskellDepends = [
    base directory filepath QuickCheck quickcheck-instances unix
  ];
  homepage = "https://github.com/ndmitchell/extra#readme";
  description = "Extra functions I use";
  license = lib.licenses.bsd3;
}
;
}
;
  filepattern = {
  meta = {
    sha256 = "1q5hdw3lq49fhl8qqn0gcp4bwqbqq34zvgrlxjpdf9sblrh3mkrl";
    url = "https://hackage.haskell.org";
    ver = "0.1.3";
  };
  drv = { mkDerivation, base, directory, extra, filepath, lib, QuickCheck
}:
mkDerivation {
  pname = "filepattern";
  version = "0.1.3";
  src = /nix/store/n612qndp5xk4sjbqp5wnb9ql7ymsm93r-source;
  libraryHaskellDepends = [ base directory extra filepath ];
  testHaskellDepends = [ base directory extra filepath QuickCheck ];
  homepage = "https://github.com/ndmitchell/filepattern#readme";
  description = "File path glob-like matching";
  license = lib.licenses.bsd3;
}
;
}
;
  generics-sop = {
  meta = {
    sha256 = "0ai089kly1cajn4djqnplkg2jmnapqlb3crrsyvfnadcyzc9h3km";
    url = "https://hackage.haskell.org";
    ver = "0.5.1.4";
  };
  drv = { mkDerivation, base, criterion, deepseq, ghc-prim, lib, sop-core
, template-haskell, th-abstraction
}:
mkDerivation {
  pname = "generics-sop";
  version = "0.5.1.4";
  src = /nix/store/qky7s4rv2qdyxl5wx3jbd5c46j7bglrx-source;
  libraryHaskellDepends = [
    base ghc-prim sop-core template-haskell th-abstraction
  ];
  testHaskellDepends = [ base ];
  benchmarkHaskellDepends = [
    base criterion deepseq template-haskell
  ];
  description = "Generic Programming using True Sums of Products";
  license = lib.licenses.bsd3;
}
;
}
;
  hedgehog = {
  meta = {
    sha256 = "1hz8xrg5p6vplvcj8c7pgidqnwqjmqahs9dla50nqpbcbdh932ll";
    url = "https://hackage.haskell.org";
    ver = "1.5";
  };
  drv = { mkDerivation, ansi-terminal, async, barbies, base, bytestring
, concurrent-output, containers, deepseq, directory, erf
, exceptions, lib, lifted-async, mmorph, monad-control, mtl
, pretty-show, primitive, random, resourcet, safe-exceptions, stm
, template-haskell, text, time, transformers, transformers-base
, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "1.5";
  src = /nix/store/asphc2qzd1cykd892r5fnhflbd8cwana-source;
  libraryHaskellDepends = [
    ansi-terminal async barbies base bytestring concurrent-output
    containers deepseq directory erf exceptions lifted-async mmorph
    monad-control mtl pretty-show primitive random resourcet
    safe-exceptions stm template-haskell text time transformers
    transformers-base wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers mmorph mtl pretty-show text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Release with confidence";
  license = lib.licenses.bsd3;
}
;
}
;
  http-client = {
  meta = {
    sha256 = "1qciglcaik1a96flj07fhqx2h692kgcv63hinffr35ka22wrg3i9";
    url = "https://hackage.haskell.org";
    ver = "0.7.19";
  };
  drv = { mkDerivation, array, async, base, base64-bytestring
, blaze-builder, bytestring, case-insensitive, containers, cookie
, deepseq, directory, exceptions, filepath, ghc-prim, hspec
, hspec-discover, http-types, iproute, lib, mime-types
, monad-control, network, network-uri, random, stm
, streaming-commons, text, time, transformers, zlib
}:
mkDerivation {
  pname = "http-client";
  version = "0.7.19";
  src = /nix/store/62hi01g26a69zq8zj61cx5xhbg3fdy1g-source;
  libraryHaskellDepends = [
    array async base base64-bytestring blaze-builder bytestring
    case-insensitive containers cookie deepseq exceptions filepath
    ghc-prim http-types iproute mime-types network network-uri random
    stm streaming-commons text time transformers
  ];
  testHaskellDepends = [
    async base blaze-builder bytestring case-insensitive containers
    cookie deepseq directory hspec http-types monad-control network
    network-uri streaming-commons text time transformers zlib
  ];
  testToolDepends = [ hspec-discover ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "An HTTP client engine";
  license = lib.licenses.mit;
}
;
}
;
  http-client-tls = {
  meta = {
    sha256 = "1f8br94l5kywpsfvpmw54b1v6nx1yapslzrwiswsf6vf8kwfjjzg";
    url = "https://hackage.haskell.org";
    ver = "0.3.6.4";
  };
  drv = { mkDerivation, base, bytestring, case-insensitive, containers
, crypton, crypton-connection, data-default, exceptions, gauge
, hspec, http-client, http-types, lib, memory, network, network-uri
, text, tls, transformers
}:
mkDerivation {
  pname = "http-client-tls";
  version = "0.3.6.4";
  src = /nix/store/8r1b74si7yr0bxiw2wp65ypxzrdsmsxk-source;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers crypton
    crypton-connection data-default exceptions http-client http-types
    memory network network-uri text tls transformers
  ];
  testHaskellDepends = [
    base crypton-connection hspec http-client http-types
  ];
  benchmarkHaskellDepends = [ base gauge http-client ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "http-client backend using the connection package and tls library";
  license = lib.licenses.mit;
}
;
}
;
  incipit-base = {
  meta = {
    sha256 = "08ybv7j94yyznrxnrh744bi3i1a00sz8bf5ddfs9vfgfhhkrg8fn";
    url = "https://hackage.haskell.org";
    ver = "0.6.1.1";
  };
  drv = { mkDerivation, base, bytestring, containers, data-default, lib
, stm, text
}:
mkDerivation {
  pname = "incipit-base";
  version = "0.6.1.1";
  src = /nix/store/z2v8hbdbz6fvdnnqfdr713164wc7n3jj-source;
  libraryHaskellDepends = [
    base bytestring containers data-default stm text
  ];
  homepage = "https://github.com/tek/incipit-core#readme";
  description = "A Prelude for Polysemy – Base Reexports";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  lens = {
  meta = {
    sha256 = "17xmlkm15681di3lvzrmcy67qmkdqqhqym040g0fdmsy05777al3";
    url = "https://hackage.haskell.org";
    ver = "5.3.4";
  };
  drv = { mkDerivation, array, assoc, base, base-orphans, bifunctors
, bytestring, call-stack, comonad, containers, contravariant
, criterion, deepseq, distributive, exceptions, filepath, free
, generic-deriving, ghc-prim, hashable, HUnit, indexed-traversable
, indexed-traversable-instances, kan-extensions, lib, mtl, parallel
, profunctors, QuickCheck, reflection, semigroupoids
, simple-reflect, strict, tagged, template-haskell, test-framework
, test-framework-hunit, test-framework-quickcheck2, text
, th-abstraction, these, transformers, transformers-compat
, unordered-containers, vector
}:
mkDerivation {
  pname = "lens";
  version = "5.3.4";
  src = /nix/store/6jbm44yacy4pp34lhi25l7hflk4yhvbc-source;
  libraryHaskellDepends = [
    array assoc base base-orphans bifunctors bytestring call-stack
    comonad containers contravariant distributive exceptions filepath
    free ghc-prim hashable indexed-traversable
    indexed-traversable-instances kan-extensions mtl parallel
    profunctors reflection semigroupoids strict tagged template-haskell
    text th-abstraction these transformers transformers-compat
    unordered-containers vector
  ];
  testHaskellDepends = [
    base bytestring containers deepseq HUnit mtl QuickCheck
    simple-reflect test-framework test-framework-hunit
    test-framework-quickcheck2 text transformers
  ];
  benchmarkHaskellDepends = [
    base bytestring comonad containers criterion deepseq
    generic-deriving transformers unordered-containers vector
  ];
  homepage = "http://github.com/ekmett/lens/";
  description = "Lenses, Folds and Traversals";
  license = lib.licenses.bsd2;
}
;
}
;
  lens-regex-pcre = {
  meta = {
    sha256 = "08wp1jwq6zdhapmrw6gmksp2slycky5rzgknsixwbrqyp11sq1vk";
    url = "https://hackage.haskell.org";
    ver = "1.1.2.0";
  };
  drv = { mkDerivation, base, bytestring, containers, hspec, lens, lib
, pcre-heavy, pcre-light, template-haskell, text
}:
mkDerivation {
  pname = "lens-regex-pcre";
  version = "1.1.2.0";
  src = /nix/store/rv2prfi520d3gsdkvv3ibr46xcrcmqr6-source;
  libraryHaskellDepends = [
    base bytestring containers lens pcre-heavy pcre-light
    template-haskell text
  ];
  testHaskellDepends = [
    base bytestring containers hspec lens pcre-heavy pcre-light
    template-haskell text
  ];
  homepage = "https://github.com/ChrisPenner/lens-regex-pcre#readme";
  description = "A lensy interface to regular expressions";
  license = lib.licenses.bsd3;
}
;
}
;
  path = {
  meta = {
    sha256 = "16hgrkvd27c9vp5447d1dv3b3fi0fv0jfig10h2j37mzk4850wg8";
    url = "https://hackage.haskell.org";
    ver = "0.9.6";
  };
  drv = { mkDerivation, aeson, base, bytestring, deepseq, exceptions
, filepath, genvalidity, genvalidity-hspec, hashable, hspec, lib
, QuickCheck, template-haskell, text, validity-bytestring
}:
mkDerivation {
  pname = "path";
  version = "0.9.6";
  src = /nix/store/17x0d7bdy3wg6nq9zw20ndi417gy13ck-source;
  libraryHaskellDepends = [
    aeson base deepseq exceptions filepath hashable template-haskell
    text
  ];
  testHaskellDepends = [
    aeson base bytestring exceptions filepath genvalidity
    genvalidity-hspec hspec QuickCheck template-haskell
    validity-bytestring
  ];
  doHaddock = false;
  description = "Support for well-typed paths";
  license = lib.licenses.bsd3;
}
;
}
;
  path-io = {
  meta = {
    sha256 = "063ma7gzqr5c6s8a1yv72jgll3xdajvgclbc8w0ddmqgcrb62x2k";
    url = "https://hackage.haskell.org";
    ver = "1.8.2";
  };
  drv = { mkDerivation, base, containers, directory, dlist, exceptions
, filepath, hspec, lib, path, temporary, time, transformers
, unix-compat
}:
mkDerivation {
  pname = "path-io";
  version = "1.8.2";
  src = /nix/store/y2n6qszdsqdfhhbw4fl146qzyj1sa7zb-source;
  libraryHaskellDepends = [
    base containers directory dlist exceptions filepath path temporary
    time transformers unix-compat
  ];
  testHaskellDepends = [ base exceptions hspec path unix-compat ];
  homepage = "https://github.com/mrkkrp/path-io";
  description = "Interface to ‘directory’ package for users of ‘path’";
  license = lib.licenses.bsd3;
}
;
}
;
  pcre-heavy = {
  meta = {
    sha256 = "0k1c0j69cpyzxyfq4q92p74rlzak6za4n0xv5b7npapml4m7kb1j";
    url = "https://hackage.haskell.org";
    ver = "1.0.0.3";
  };
  drv = { mkDerivation, base, base-compat, bytestring, doctest, Glob, lib
, pcre-light, semigroups, string-conversions, template-haskell
}:
mkDerivation {
  pname = "pcre-heavy";
  version = "1.0.0.3";
  src = /nix/store/wjfrvlhvbbr5l7ich45sqvdiax66f51k-source;
  libraryHaskellDepends = [
    base base-compat bytestring pcre-light semigroups
    string-conversions template-haskell
  ];
  testHaskellDepends = [ base doctest Glob ];
  homepage = "https://codeberg.org/valpackett/pcre-heavy";
  description = "A regexp (regex) library on top of pcre-light you can actually use";
  license = lib.licenses.publicDomain;
}
;
}
;
  tasty = {
  meta = {
    sha256 = "1xjlmgsww34asjl9rcwbziw5l4qqbvi5l4b7qvzf4dc7hqkpq1rs";
    url = "https://hackage.haskell.org";
    ver = "1.5.3";
  };
  drv = { mkDerivation, ansi-terminal, base, containers, lib
, optparse-applicative, stm, tagged, transformers, unix
}:
mkDerivation {
  pname = "tasty";
  version = "1.5.3";
  src = /nix/store/9028fgac7afc6vw6is37lvq4p8gqpa7m-source;
  libraryHaskellDepends = [
    ansi-terminal base containers optparse-applicative stm tagged
    transformers unix
  ];
  homepage = "https://github.com/UnkindPartition/tasty";
  description = "Modern and extensible testing framework";
  license = lib.licenses.mit;
}
;
}
;
  tasty-hedgehog = {
  meta = {
    sha256 = "04kg2qdnsqzzmj3xggy2jcgidlp21lsjkz4sfnbq7b1yhrv2vbbc";
    url = "https://hackage.haskell.org";
    ver = "1.4.0.2";
  };
  drv = { mkDerivation, base, hedgehog, lib, tagged, tasty
, tasty-expected-failure
}:
mkDerivation {
  pname = "tasty-hedgehog";
  version = "1.4.0.2";
  src = /nix/store/b9mxq4fh65sif22q9a4g041jvp847cyc-source;
  libraryHaskellDepends = [ base hedgehog tagged tasty ];
  testHaskellDepends = [
    base hedgehog tasty tasty-expected-failure
  ];
  homepage = "https://github.com/qfpl/tasty-hedgehog";
  description = "Integration for tasty and hedgehog";
  license = lib.licenses.bsd3;
}
;
}
;
  typed-process = {
  meta = {
    sha256 = "06ysyzxvlkn1fhd0nxk0q9dcy9vrdqj7c51bv9x33gjbxbaqnfs3";
    url = "https://hackage.haskell.org";
    ver = "0.2.13.0";
  };
  drv = { mkDerivation, async, base, base64-bytestring, bytestring, hspec
, hspec-discover, lib, process, stm, temporary, text, transformers
, unliftio-core
}:
mkDerivation {
  pname = "typed-process";
  version = "0.2.13.0";
  src = /nix/store/7a0pbalinl2kfsv29ld50afdiynkf285-source;
  libraryHaskellDepends = [
    async base bytestring process stm text transformers unliftio-core
  ];
  testHaskellDepends = [
    async base base64-bytestring bytestring hspec process stm temporary
    text transformers unliftio-core
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/fpco/typed-process";
  description = "Run external processes, with strong typing of streams";
  license = lib.licenses.mit;
}
;
}
;
};
lower = {
  aeson = {
  meta = {
    sha256 = "1f1f6h2r60ghz4p1ddi6wnq6z3i07j60sgm77hx2rvmncz4vizp0";
    url = "https://hackage.haskell.org";
    ver = "2.1.2.1";
  };
  drv = { mkDerivation, attoparsec, base, base-compat
, base-compat-batteries, base-orphans, base16-bytestring
, bytestring, containers, data-fix, deepseq, Diff, directory, dlist
, exceptions, filepath, generic-deriving, generically, ghc-prim
, hashable, indexed-traversable, integer-logarithms, lib, OneTuple
, primitive, QuickCheck, quickcheck-instances, scientific
, semialign, strict, tagged, tasty, tasty-golden, tasty-hunit
, tasty-quickcheck, template-haskell, text, text-short
, th-abstraction, these, time, time-compat, unordered-containers
, uuid-types, vector, witherable
}:
mkDerivation {
  pname = "aeson";
  version = "2.1.2.1";
  src = /nix/store/hdckfxcpamn3qv2a0xn9pfzz64k0fpvp-source;
  libraryHaskellDepends = [
    attoparsec base base-compat-batteries bytestring containers
    data-fix deepseq dlist exceptions generically ghc-prim hashable
    indexed-traversable OneTuple primitive QuickCheck scientific
    semialign strict tagged template-haskell text text-short
    th-abstraction these time time-compat unordered-containers
    uuid-types vector witherable
  ];
  testHaskellDepends = [
    attoparsec base base-compat base-orphans base16-bytestring
    bytestring containers data-fix Diff directory dlist filepath
    generic-deriving generically ghc-prim hashable indexed-traversable
    integer-logarithms OneTuple primitive QuickCheck
    quickcheck-instances scientific strict tagged tasty tasty-golden
    tasty-hunit tasty-quickcheck template-haskell text text-short these
    time time-compat unordered-containers uuid-types vector
  ];
  homepage = "https://github.com/haskell/aeson";
  description = "Fast JSON parsing and encoding";
  license = lib.licenses.bsd3;
}
;
}
;
  ansi-wl-pprint = {
  meta = {
    sha256 = "08akbbdra1sx36ff1la5k7rcxlz543i86qk4gyyxbxy636m9fhwv";
    url = "https://hackage.haskell.org";
    ver = "0.6.9";
  };
  drv = { mkDerivation, ansi-terminal, base, lib }:
mkDerivation {
  pname = "ansi-wl-pprint";
  version = "0.6.9";
  src = /nix/store/z4qb2zjir7qiqdv4fl79iys17ig6vl8g-source;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ ansi-terminal base ];
  homepage = "http://github.com/ekmett/ansi-wl-pprint";
  description = "The Wadler/Leijen Pretty Printer for colored ANSI terminal output";
  license = lib.licenses.bsd3;
}
;
}
;
  casing = {
  meta = {
    sha256 = "1pm50h7223d08rk7n2b79i1h8pc3fp7gvis0a4829xms1ysn280h";
    url = "https://hackage.haskell.org";
    ver = "0.1.0.0";
  };
  drv = { mkDerivation, base, lib, split }:
mkDerivation {
  pname = "casing";
  version = "0.1.0.0";
  src = /nix/store/jqxsnr58yc5hn4mxw9zxc57k923x7q1y-source;
  libraryHaskellDepends = [ base split ];
  description = "Convert between various source code casing conventions";
  license = lib.licenses.mit;
}
;
}
;
  connection = {
  meta = {
    sha256 = "0qjdz2fxxszbns7cszhnkwm8x8l3xlnad6iydx2snfi416sypiy0";
    url = "https://hackage.haskell.org";
    ver = "0.3.1";
  };
  drv = { mkDerivation, base, basement, bytestring, containers
, data-default-class, lib, network, socks, tls, x509, x509-store
, x509-system, x509-validation
}:
mkDerivation {
  pname = "connection";
  version = "0.3.1";
  src = /nix/store/z6pnb23wcx360hrsspcvy9mcknp5v7b3-source;
  libraryHaskellDepends = [
    base basement bytestring containers data-default-class network
    socks tls x509 x509-store x509-system x509-validation
  ];
  homepage = "https://github.com/vincenthz/hs-connection";
  description = "Simple and easy network connections API";
  license = lib.licenses.bsd3;
}
;
}
;
  cryptonite = {
  meta = {
    sha256 = "04wq8lh300dng87n59a37ngjqbwjlxpd62vz6ifvz0gpyx0pnhy7";
    url = "https://hackage.haskell.org";
    ver = "0.30";
  };
  drv = { mkDerivation, base, basement, bytestring, deepseq, gauge
, ghc-prim, integer-gmp, lib, memory, random, tasty, tasty-hunit
, tasty-kat, tasty-quickcheck
}:
mkDerivation {
  pname = "cryptonite";
  version = "0.30";
  src = /nix/store/vg0dva7p4r7zaz2x2pb79psj4i8azlns-source;
  libraryHaskellDepends = [
    base basement bytestring deepseq ghc-prim integer-gmp memory
  ];
  testHaskellDepends = [
    base bytestring memory tasty tasty-hunit tasty-kat tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    base bytestring deepseq gauge memory random
  ];
  homepage = "https://github.com/haskell-crypto/cryptonite";
  description = "Cryptography Primitives sink";
  license = lib.licenses.bsd3;
}
;
}
;
  exon = {
  meta = {
    sha256 = "1m4i3a14wip985ncblfy2ikcy7gw5rryj9z497ah218d1nmwj7rl";
    url = "https://hackage.haskell.org";
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
  extra = {
  meta = {
    sha256 = "1b7gw130gpkhjp7fhj99xkkwhzxnw799s4r8appzbdvj62hm36n5";
    url = "https://hackage.haskell.org";
    ver = "1.7.11";
  };
  drv = { mkDerivation, base, clock, directory, filepath, lib, process
, QuickCheck, quickcheck-instances, time, unix
}:
mkDerivation {
  pname = "extra";
  version = "1.7.11";
  src = /nix/store/5h1b5lmmn10bk5l8sgx8n6qdg9wq0p6h-source;
  libraryHaskellDepends = [
    base clock directory filepath process time unix
  ];
  testHaskellDepends = [
    base directory filepath QuickCheck quickcheck-instances unix
  ];
  homepage = "https://github.com/ndmitchell/extra#readme";
  description = "Extra functions I use";
  license = lib.licenses.bsd3;
}
;
}
;
  filepattern = {
  meta = {
    sha256 = "165nh98rbmi9cwnjrcll1mgy1h0jkvylv8z980sp5kfrrmcjay33";
    url = "https://hackage.haskell.org";
    ver = "0.1";
  };
  drv = { mkDerivation, base, directory, extra, filepath, lib, QuickCheck
}:
mkDerivation {
  pname = "filepattern";
  version = "0.1";
  src = /nix/store/4jwid1p5iza0xnx97jcjlxdmjp4p05ss-source;
  libraryHaskellDepends = [ base directory extra filepath ];
  testHaskellDepends = [ base directory extra filepath QuickCheck ];
  description = "File path glob-like matching";
  license = lib.licenses.bsd3;
}
;
}
;
  flatparse = {
  meta = {
    sha256 = "0z9q5qb3yz4phvj1wq06dld745m98yk4gvkmj1vkq7hda0mn182a";
    url = "https://hackage.haskell.org";
    ver = "0.4.1.0";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, containers, gauge
, hspec, HUnit, integer-gmp, lib, megaparsec, parsec, primitive
, QuickCheck, quickcheck-instances, template-haskell, utf8-string
}:
mkDerivation {
  pname = "flatparse";
  version = "0.4.1.0";
  src = /nix/store/8cqbb3d6f4x8g2knirf5v3fcjjydqxmy-source;
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
  generic-lens = {
  meta = {
    sha256 = "0cd3w5hpf0yqi1vrkxzm4qlc2n797fdmhyhcvkrz4ym19v2vylyb";
    url = "https://hackage.haskell.org";
    ver = "2.2.1.0";
  };
  drv = { mkDerivation, base, doctest, generic-lens-core, HUnit
, inspection-testing, lens, lib, profunctors, text
}:
mkDerivation {
  pname = "generic-lens";
  version = "2.2.1.0";
  src = /nix/store/cz1714jzkizb6mnqfvr4n57n6bns9ffl-source;
  libraryHaskellDepends = [
    base generic-lens-core profunctors text
  ];
  testHaskellDepends = [
    base doctest HUnit inspection-testing lens profunctors
  ];
  homepage = "https://github.com/kcsongor/generic-lens";
  description = "Generically derive traversals, lenses and prisms";
  license = lib.licenses.bsd3;
}
;
}
;
  ghc-hs-meta = {
  meta = {
    sha256 = "19z2704dl6x4lkgfaynhn550wdghpj9qdwh5xr96drp3nkh012dl";
    url = "https://hackage.haskell.org";
    ver = "0.1.5.0";
  };
  drv = { mkDerivation, base, bytestring, ghc, ghc-boot, hspec, lib
, template-haskell
}:
mkDerivation {
  pname = "ghc-hs-meta";
  version = "0.1.5.0";
  src = /nix/store/7abpm6lm194m0f4xd576kc9lf2qp7py3-source;
  libraryHaskellDepends = [
    base bytestring ghc ghc-boot template-haskell
  ];
  testHaskellDepends = [
    base bytestring ghc ghc-boot hspec template-haskell
  ];
  description = "Translate Haskell source to Template Haskell expression";
  license = lib.licenses.bsd3;
}
;
}
;
  hedgehog = {
  meta = {
    sha256 = "1ayqg1qxxvbq4h1n801b207j378mzqhlbvhcqfgfb4ikq57lxzir";
    url = "https://hackage.haskell.org";
    ver = "1.3";
  };
  drv = { mkDerivation, ansi-terminal, async, barbies, base, bytestring
, concurrent-output, containers, deepseq, directory, erf
, exceptions, lib, lifted-async, mmorph, monad-control, mtl
, pretty-show, primitive, random, resourcet, safe-exceptions, stm
, template-haskell, text, time, transformers, transformers-base
, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "1.3";
  src = /nix/store/ymbrp1hr88ilpbbvwdiwi6w7i4zkymik-source;
  libraryHaskellDepends = [
    ansi-terminal async barbies base bytestring concurrent-output
    containers deepseq directory erf exceptions lifted-async mmorph
    monad-control mtl pretty-show primitive random resourcet
    safe-exceptions stm template-haskell text time transformers
    transformers-base wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers mmorph mtl pretty-show text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Release with confidence";
  license = lib.licenses.bsd3;
}
;
}
;
  http-client = {
  meta = {
    sha256 = "0zqxz8n24hbpgib8ljbx5xyf6274xzzqrgd1ydf1cwykz9lgvvwd";
    url = "https://hackage.haskell.org";
    ver = "0.5.11";
  };
  drv = { mkDerivation, array, async, base, blaze-builder, bytestring
, case-insensitive, containers, cookie, deepseq, directory
, exceptions, filepath, ghc-prim, hspec, http-types, lib, memory
, mime-types, monad-control, network, network-uri, random, stm
, streaming-commons, text, time, transformers, zlib
}:
mkDerivation {
  pname = "http-client";
  version = "0.5.11";
  src = /nix/store/ldlf7lh7yz1gc0kiws6f7wpgdxndz921-source;
  libraryHaskellDepends = [
    array base blaze-builder bytestring case-insensitive containers
    cookie deepseq exceptions filepath ghc-prim http-types memory
    mime-types network network-uri random stm streaming-commons text
    time transformers
  ];
  testHaskellDepends = [
    async base blaze-builder bytestring case-insensitive containers
    deepseq directory hspec http-types monad-control network
    network-uri streaming-commons text time transformers zlib
  ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "An HTTP client engine";
  license = lib.licenses.mit;
}
;
}
;
  http-client-tls = {
  meta = {
    sha256 = "08rwd8xmiq4ppxfzid9x01vr9b5sq7zbzc7kad1gashh4ybfd921";
    url = "https://hackage.haskell.org";
    ver = "0.3.0";
  };
  drv = { mkDerivation, base, bytestring, connection, data-default-class
, hspec, http-client, http-types, lib, network, tls
}:
mkDerivation {
  pname = "http-client-tls";
  version = "0.3.0";
  src = /nix/store/hg2f8g4f7wmd8864621hmh32wf1vg3rh-source;
  libraryHaskellDepends = [
    base bytestring connection data-default-class http-client network
    tls
  ];
  testHaskellDepends = [ base hspec http-client http-types ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "http-client backend using the connection package and tls library";
  license = lib.licenses.mit;
}
;
}
;
  http-types = {
  meta = {
    sha256 = "0x0wpfqm5chpskq6jm1yyfwyzf8w6hk9cf26vsyhbqj991r4lvq9";
    url = "https://hackage.haskell.org";
    ver = "0.11";
  };
  drv = { mkDerivation, array, base, bytestring, case-insensitive, doctest
, hspec, lib, QuickCheck, quickcheck-instances, text
}:
mkDerivation {
  pname = "http-types";
  version = "0.11";
  src = /nix/store/jr7i9qddczw3cyvaahcaaz9kz0fxis5k-source;
  libraryHaskellDepends = [
    array base bytestring case-insensitive text
  ];
  testHaskellDepends = [
    base bytestring doctest hspec QuickCheck quickcheck-instances text
  ];
  homepage = "https://github.com/aristidb/http-types";
  description = "Generic HTTP types for Haskell (for both client and server code)";
  license = lib.licenses.bsd3;
}
;
}
;
  lens = {
  meta = {
    sha256 = "0wvd9qvhaajb2dlaj7761q5c4ni8wx0kk7v5ix25bl5npgngq1p7";
    url = "https://hackage.haskell.org";
    ver = "5.2.1";
  };
  drv = { mkDerivation, array, assoc, base, base-compat, base-orphans
, bifunctors, bytestring, call-stack, comonad, containers
, contravariant, criterion, deepseq, distributive, exceptions
, filepath, free, generic-deriving, ghc-prim, hashable, HUnit
, indexed-traversable, indexed-traversable-instances
, kan-extensions, lib, mtl, parallel, profunctors, QuickCheck
, reflection, semigroupoids, simple-reflect, strict, tagged
, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, th-abstraction, these
, transformers, transformers-compat, unordered-containers, vector
}:
mkDerivation {
  pname = "lens";
  version = "5.2.1";
  src = /nix/store/zjbrn7vpgwsixm0zwmyhnn0iq32ndckn-source;
  libraryHaskellDepends = [
    array assoc base base-orphans bifunctors bytestring call-stack
    comonad containers contravariant distributive exceptions filepath
    free ghc-prim hashable indexed-traversable
    indexed-traversable-instances kan-extensions mtl parallel
    profunctors reflection semigroupoids strict tagged template-haskell
    text th-abstraction these transformers transformers-compat
    unordered-containers vector
  ];
  testHaskellDepends = [
    base containers deepseq HUnit mtl QuickCheck simple-reflect
    test-framework test-framework-hunit test-framework-quickcheck2
    transformers
  ];
  benchmarkHaskellDepends = [
    base base-compat bytestring comonad containers criterion deepseq
    generic-deriving transformers unordered-containers vector
  ];
  homepage = "http://github.com/ekmett/lens/";
  description = "Lenses, Folds and Traversals";
  license = lib.licenses.bsd2;
}
;
}
;
  lens-regex-pcre = {
  meta = {
    sha256 = "1m9jpyhsa55zd064c0mqw0aan01sxwwp3i7f7p5gbrbi72w2qaw4";
    url = "https://hackage.haskell.org";
    ver = "1.0.0.0";
  };
  drv = { mkDerivation, base, bytestring, gauge, hspec, lens, lib
, pcre-heavy, template-haskell, text
}:
mkDerivation {
  pname = "lens-regex-pcre";
  version = "1.0.0.0";
  src = /nix/store/zas1dpcaw2mzf3p4shhhjrykn2csxrm5-source;
  libraryHaskellDepends = [
    base bytestring lens pcre-heavy template-haskell text
  ];
  testHaskellDepends = [
    base bytestring hspec lens pcre-heavy template-haskell text
  ];
  benchmarkHaskellDepends = [
    base bytestring gauge lens pcre-heavy template-haskell text
  ];
  homepage = "https://github.com/ChrisPenner/lens-regex-pcre#readme";
  description = "A lensy interface to regular expressions";
  license = lib.licenses.bsd3;
}
;
}
;
  optparse-applicative = {
  meta = {
    sha256 = "1jmaizbpksnaf89afz8yz3phnjmyrzch2r22ir8ydw22f6zd5srk";
    url = "https://hackage.haskell.org";
    ver = "0.16.1.0";
  };
  drv = { mkDerivation, ansi-wl-pprint, base, lib, process, QuickCheck
, transformers, transformers-compat
}:
mkDerivation {
  pname = "optparse-applicative";
  version = "0.16.1.0";
  src = /nix/store/gz5a20kw0w49zqq3ip5zxrx4x5v0qnl4-source;
  libraryHaskellDepends = [
    ansi-wl-pprint base process transformers transformers-compat
  ];
  testHaskellDepends = [ base QuickCheck ];
  homepage = "https://github.com/pcapriotti/optparse-applicative";
  description = "Utilities and combinators for parsing command line options";
  license = lib.licenses.bsd3;
}
;
}
;
  path = {
  meta = {
    sha256 = "0nk3rp5fk97m4y163dyd1y488062djzj071xdd90yyghi5pgvrb5";
    url = "https://hackage.haskell.org";
    ver = "0.9.1";
  };
  drv = { mkDerivation, aeson, base, bytestring, deepseq, exceptions
, filepath, genvalidity, genvalidity-hspec, genvalidity-property
, hashable, hspec, lib, mtl, QuickCheck, template-haskell, text
, validity
}:
mkDerivation {
  pname = "path";
  version = "0.9.1";
  src = /nix/store/9pr85w0qq87ak44gdyqgnarmv5ppdqga-source;
  libraryHaskellDepends = [
    aeson base deepseq exceptions filepath hashable template-haskell
    text
  ];
  testHaskellDepends = [
    aeson base bytestring filepath genvalidity genvalidity-hspec
    genvalidity-property hspec mtl QuickCheck template-haskell validity
  ];
  description = "Support for well-typed paths";
  license = lib.licenses.bsd3;
}
;
}
;
  path-io = {
  meta = {
    sha256 = "05hcxgyf6kkz36mazd0fqwb6mjy2049gx3vh8qq9h93gfjkpp2vc";
    url = "https://hackage.haskell.org";
    ver = "1.6.3";
  };
  drv = { mkDerivation, base, containers, directory, dlist, exceptions
, filepath, hspec, lib, path, temporary, time, transformers
, unix-compat
}:
mkDerivation {
  pname = "path-io";
  version = "1.6.3";
  src = /nix/store/vgfbjck2brpd6zb090ljasw6z2xgvif9-source;
  libraryHaskellDepends = [
    base containers directory dlist exceptions filepath path temporary
    time transformers unix-compat
  ];
  testHaskellDepends = [
    base directory exceptions filepath hspec path transformers
    unix-compat
  ];
  homepage = "https://github.com/mrkkrp/path-io";
  description = "Interface to ‘directory’ package for users of ‘path’";
  license = lib.licenses.bsd3;
}
;
}
;
  semialign = {
  meta = {
    sha256 = "09147lz8z79ghnpr7z08y9dd0l6c9dz9hz85avfh6z330vl25r95";
    url = "https://hackage.haskell.org";
    ver = "1.3";
  };
  drv = { mkDerivation, base, containers, hashable, indexed-traversable
, indexed-traversable-instances, lib, semigroupoids, tagged, these
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "semialign";
  version = "1.3";
  src = /nix/store/010v4lpl04wiffljasal5rg7sqqcw8sh-source;
  libraryHaskellDepends = [
    base containers hashable indexed-traversable
    indexed-traversable-instances semigroupoids tagged these
    transformers unordered-containers vector
  ];
  homepage = "https://github.com/haskellari/these";
  description = "Align and Zip type-classes from the common Semialign ancestor";
  license = lib.licenses.bsd3;
}
;
}
;
  strict = {
  meta = {
    sha256 = "02iyvrr7nd7fnivz78lzdchy8zw1cghqj1qx2yzbbb9869h1mny7";
    url = "https://hackage.haskell.org";
    ver = "0.5";
  };
  drv = { mkDerivation, assoc, base, binary, bytestring, deepseq, ghc-prim
, hashable, lib, text, these, transformers
}:
mkDerivation {
  pname = "strict";
  version = "0.5";
  src = /nix/store/21ahwfbr944xz0c7cs47f6z86p78plps-source;
  libraryHaskellDepends = [
    assoc base binary bytestring deepseq ghc-prim hashable text these
    transformers
  ];
  homepage = "https://github.com/haskell-strict/strict";
  description = "Strict data types and String IO";
  license = lib.licenses.bsd3;
}
;
}
;
  tasty = {
  meta = {
    sha256 = "0ijgjmjgzy6g9kp9665m4clhsfjhqdbhn2r4ppvgm6g137d2dr8p";
    url = "https://hackage.haskell.org";
    ver = "1.4.2.2";
  };
  drv = { mkDerivation, ansi-terminal, base, clock, containers, lib, mtl
, optparse-applicative, stm, tagged, unbounded-delays, unix
, wcwidth
}:
mkDerivation {
  pname = "tasty";
  version = "1.4.2.2";
  src = /nix/store/pvbjdkn7dd7iaz1gjvv3yxrxkkrx0fmx-source;
  libraryHaskellDepends = [
    ansi-terminal base clock containers mtl optparse-applicative stm
    tagged unbounded-delays unix wcwidth
  ];
  homepage = "https://github.com/UnkindPartition/tasty";
  description = "Modern and extensible testing framework";
  license = lib.licenses.mit;
}
;
}
;
  tasty-hedgehog = {
  meta = {
    sha256 = "0bxpmi2nyn84zscnhi4bnhza4l7kjgddc9z7nr227iq2q0w56l1g";
    url = "https://hackage.haskell.org";
    ver = "1.4.0.1";
  };
  drv = { mkDerivation, base, hedgehog, lib, tagged, tasty
, tasty-expected-failure
}:
mkDerivation {
  pname = "tasty-hedgehog";
  version = "1.4.0.1";
  src = /nix/store/kifv3qy59viwzz0achm13fwx6l3lcfs4-source;
  libraryHaskellDepends = [ base hedgehog tagged tasty ];
  testHaskellDepends = [
    base hedgehog tasty tasty-expected-failure
  ];
  homepage = "https://github.com/qfpl/tasty-hedgehog";
  description = "Integration for tasty and hedgehog";
  license = lib.licenses.bsd3;
}
;
}
;
  these = {
  meta = {
    sha256 = "1iaaq1fsvg8c3l0czcicshkmbbr00hnwkdamjbkljsa1qvlilaf0";
    url = "https://hackage.haskell.org";
    ver = "1.2";
  };
  drv = { mkDerivation, assoc, base, binary, deepseq, hashable, lib }:
mkDerivation {
  pname = "these";
  version = "1.2";
  src = /nix/store/mqzsrdx9ic8drb7yv2vxnjfsa0mmr0am-source;
  libraryHaskellDepends = [ assoc base binary deepseq hashable ];
  homepage = "https://github.com/haskellari/these";
  description = "An either-or-both data type";
  license = lib.licenses.bsd3;
}
;
}
;
  tls = {
  meta = {
    sha256 = "1bw3y5bxxpa3xncvh6g8z8x5gxz8kjl2didk229m7l79jvcicijy";
    url = "https://hackage.haskell.org";
    ver = "1.6.0";
  };
  drv = { mkDerivation, asn1-encoding, asn1-types, async, base, bytestring
, cereal, cryptonite, data-default-class, gauge, hourglass, lib
, memory, mtl, network, QuickCheck, tasty, tasty-quickcheck
, transformers, x509, x509-store, x509-validation
}:
mkDerivation {
  pname = "tls";
  version = "1.6.0";
  src = /nix/store/ycq2a7bfy01r2h4050fak5sb059ch49c-source;
  libraryHaskellDepends = [
    asn1-encoding asn1-types async base bytestring cereal cryptonite
    data-default-class hourglass memory mtl network transformers x509
    x509-store x509-validation
  ];
  testHaskellDepends = [
    asn1-types async base bytestring cryptonite data-default-class
    hourglass QuickCheck tasty tasty-quickcheck x509 x509-validation
  ];
  benchmarkHaskellDepends = [
    asn1-types async base bytestring cryptonite data-default-class
    gauge hourglass QuickCheck tasty-quickcheck x509 x509-validation
  ];
  homepage = "http://github.com/vincenthz/hs-tls";
  description = "TLS/SSL protocol native implementation (Server and Client)";
  license = lib.licenses.bsd3;
}
;
}
;
  typed-process = {
  meta = {
    sha256 = "00jgzcqc6n759547ij7s5bfb08q92sq3kfrbzhh5l1ppz5agv9li";
    url = "https://hackage.haskell.org";
    ver = "0.2.5.0";
  };
  drv = { mkDerivation, async, base, base64-bytestring, bytestring, hspec
, lib, process, stm, temporary, transformers
}:
mkDerivation {
  pname = "typed-process";
  version = "0.2.5.0";
  src = /nix/store/8jm7f7g10dj9g5v0y6886qjw5n1z3brn-source;
  libraryHaskellDepends = [
    async base bytestring process stm transformers
  ];
  testHaskellDepends = [
    async base base64-bytestring bytestring hspec process stm temporary
    transformers
  ];
  homepage = "https://haskell-lang.org/library/typed-process";
  description = "Run external processes, with strong typing of streams";
  license = lib.licenses.mit;
}
;
}
;
  unbounded-delays = {
  meta = {
    sha256 = "1kbh2yr7lwzrhjniyfllsix2zn8bmz9yrkhnq5lxv9ic9bbxnls7";
    url = "https://hackage.haskell.org";
    ver = "0.1.1.1";
  };
  drv = { mkDerivation, base, lib }:
mkDerivation {
  pname = "unbounded-delays";
  version = "0.1.1.1";
  src = /nix/store/hp6mlfj9kg8r7l4gjcak2i7zbxwjzl4s-source;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/basvandijk/unbounded-delays";
  description = "Unbounded thread delays and timeouts";
  license = lib.licenses.bsd3;
}
;
}
;
  wcwidth = {
  meta = {
    sha256 = "0131h9vg8dvrqcc2sn0k8y6cb08fazlfhr4922hwv2vbx3cnyy3z";
    url = "https://hackage.haskell.org";
    ver = "0.0.2";
  };
  drv = { mkDerivation, base, containers, lib }:
mkDerivation {
  pname = "wcwidth";
  version = "0.0.2";
  src = /nix/store/n4f5lcschfan070bj3i6fa8pwjx1p94s-source;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers ];
  homepage = "http://github.com/solidsnack/wcwidth/";
  description = "Native wcwidth";
  license = lib.licenses.bsd3;
}
;
}
;
  x509 = {
  meta = {
    sha256 = "1pld1yx0fl6g4bzqfx147xipl3kzfx6pz8q4difw2k0kg0qj6xar";
    url = "https://hackage.haskell.org";
    ver = "1.7.7";
  };
  drv = { mkDerivation, asn1-encoding, asn1-parse, asn1-types, base
, bytestring, containers, cryptonite, hourglass, lib, memory, mtl
, pem, tasty, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "x509";
  version = "1.7.7";
  src = /nix/store/s399rqyyqzq4xjqbyc6lc4585191crgq-source;
  libraryHaskellDepends = [
    asn1-encoding asn1-parse asn1-types base bytestring containers
    cryptonite hourglass memory pem transformers
  ];
  testHaskellDepends = [
    asn1-types base bytestring cryptonite hourglass mtl tasty
    tasty-quickcheck
  ];
  homepage = "http://github.com/vincenthz/hs-certificate";
  description = "X509 reader and writer";
  license = lib.licenses.bsd3;
}
;
}
;
  x509-store = {
  meta = {
    sha256 = "182hjs50x6w153j2a8k9myd2i1csvrz16hzms2ny0ygyxanqrd2z";
    url = "https://hackage.haskell.org";
    ver = "1.6.9";
  };
  drv = { mkDerivation, asn1-encoding, asn1-types, base, bytestring
, containers, cryptonite, directory, filepath, lib, mtl, pem, tasty
, tasty-hunit, x509
}:
mkDerivation {
  pname = "x509-store";
  version = "1.6.9";
  src = /nix/store/9h95pyw1n0gfg66762xnhfny0hap2359-source;
  libraryHaskellDepends = [
    asn1-encoding asn1-types base bytestring containers cryptonite
    directory filepath mtl pem x509
  ];
  testHaskellDepends = [ base bytestring tasty tasty-hunit x509 ];
  homepage = "http://github.com/vincenthz/hs-certificate";
  description = "X.509 collection accessing and storing methods";
  license = lib.licenses.bsd3;
}
;
}
;
  x509-system = {
  meta = {
    sha256 = "0ccr0n2d1vv8v36rkz2cj8ilx2vldzg4cp3rs91277x0csfawwd5";
    url = "https://hackage.haskell.org";
    ver = "1.6.7";
  };
  drv = { mkDerivation, base, bytestring, containers, directory, filepath
, lib, mtl, pem, process, x509, x509-store
}:
mkDerivation {
  pname = "x509-system";
  version = "1.6.7";
  src = /nix/store/vbvzm3glb7zbwc6bjv038xyrjkvlsmq3-source;
  libraryHaskellDepends = [
    base bytestring containers directory filepath mtl pem process x509
    x509-store
  ];
  homepage = "http://github.com/vincenthz/hs-certificate";
  description = "Handle per-operating-system X.509 accessors and storage";
  license = lib.licenses.bsd3;
}
;
}
;
  x509-validation = {
  meta = {
    sha256 = "1jrsryn6hfdmr1b1alpj5zxvw26dw8y7kqpq555q2njm3kvwmxap";
    url = "https://hackage.haskell.org";
    ver = "1.6.12";
  };
  drv = { mkDerivation, asn1-encoding, asn1-types, base, bytestring
, containers, cryptonite, data-default-class, hourglass, lib
, memory, mtl, pem, tasty, tasty-hunit, x509, x509-store
}:
mkDerivation {
  pname = "x509-validation";
  version = "1.6.12";
  src = /nix/store/in9faarmpsfvfqb79lrkvvzfas3yx8i1-source;
  libraryHaskellDepends = [
    asn1-encoding asn1-types base bytestring containers cryptonite
    data-default-class hourglass memory mtl pem x509 x509-store
  ];
  testHaskellDepends = [
    asn1-encoding asn1-types base bytestring cryptonite
    data-default-class hourglass memory tasty tasty-hunit x509
    x509-store
  ];
  homepage = "http://github.com/vincenthz/hs-certificate";
  description = "X.509 Certificate and CRL validation";
  license = lib.licenses.bsd3;
}
;
}
;
};
min = {
};
profiled = {
};
}