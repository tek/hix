{
dev = {
};
ghc912 = {
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
  generic-lens = {
  meta = {
    sha256 = "06q0ghaj90hqp0chb3z5qzr3cx8ypanjk24d4wnb1b7b8s13rhsp";
    url = "https://hackage.haskell.org";
    ver = "2.3.0.0";
  };
  drv = { mkDerivation, base, doctest, generic-lens-core, HUnit
, inspection-testing, lens, lib, mtl, profunctors
}:
mkDerivation {
  pname = "generic-lens";
  version = "2.3.0.0";
  src = /nix/store/fi8256z790q44j9l9w91qpip94gf5494-source;
  libraryHaskellDepends = [ base generic-lens-core profunctors ];
  testHaskellDepends = [
    base doctest HUnit inspection-testing lens mtl
  ];
  homepage = "https://github.com/kcsongor/generic-lens";
  description = "Generically derive traversals, lenses and prisms";
  license = lib.licenses.bsd3;
}
;
}
;
  generic-lens-core = {
  meta = {
    sha256 = "05im3y27lhjjy6hi0i85rlqsan510fmp63lqfwg18cnlzn0yvf81";
    url = "https://hackage.haskell.org";
    ver = "2.3.0.0";
  };
  drv = { mkDerivation, base, indexed-profunctors, lib, text }:
mkDerivation {
  pname = "generic-lens-core";
  version = "2.3.0.0";
  src = /nix/store/d0648wfd6zvrini3699ybcf9vzfm47z5-source;
  libraryHaskellDepends = [ base indexed-profunctors text ];
  homepage = "https://github.com/kcsongor/generic-lens";
  description = "Generically derive traversals, lenses and prisms";
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
    sha256 = "04cjnz4i1qs3v9bza8a3ry1czapwqgxazhywkjzq2rg1544gjmby";
    url = "https://hackage.haskell.org";
    ver = "1.7";
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
  version = "1.7";
  src = /nix/store/piimk6ymh2yg2m74npn5p2znh3wvard4-source;
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
  homepage = "http://github.com/hedgehogqa/haskell-hedgehog";
  description = "Release with confidence";
  license = lib.licenses.bsd3;
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
  optparse-applicative = {
  meta = {
    sha256 = "0cs8fqipakad38lvm75nz98hmvf881mgjhnc7icblxfzh92ay6kn";
    url = "https://hackage.haskell.org";
    ver = "0.19.0.0";
  };
  drv = { mkDerivation, base, lib, prettyprinter
, prettyprinter-ansi-terminal, process, QuickCheck, text
, transformers
}:
mkDerivation {
  pname = "optparse-applicative";
  version = "0.19.0.0";
  src = /nix/store/l5z3gyf61qdyda9hmv5fqdq6svb2g7wh-source;
  libraryHaskellDepends = [
    base prettyprinter prettyprinter-ansi-terminal process text
    transformers
  ];
  testHaskellDepends = [ base QuickCheck ];
  homepage = "https://github.com/pcapriotti/optparse-applicative";
  description = "Utilities and combinators for parsing command line options";
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
  tasty-quickcheck = {
  meta = {
    sha256 = "1sb16pchr52kaw8vpzczjf2j4kjp05x0qkx3619djaywzcsnfpgx";
    url = "https://hackage.haskell.org";
    ver = "0.11";
  };
  drv = { mkDerivation, base, lib, optparse-applicative, pcre-light
, QuickCheck, random, tagged, tasty, tasty-hunit
}:
mkDerivation {
  pname = "tasty-quickcheck";
  version = "0.11";
  src = /nix/store/84mcz809kv1ykkwk555pabv8cvg8wjql-source;
  libraryHaskellDepends = [
    base optparse-applicative QuickCheck random tagged tasty
  ];
  testHaskellDepends = [
    base pcre-light QuickCheck tasty tasty-hunit
  ];
  homepage = "https://github.com/UnkindPartition/tasty";
  description = "QuickCheck support for the Tasty test framework";
  license = lib.licenses.mit;
}
;
}
;
};
lower = {
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
  cookie = {
  meta = {
    sha256 = "1nzgswdbnrv69psngx3yz6axbigy7kr4na9zy09dn5q7im7i9hfj";
    url = "https://hackage.haskell.org";
    ver = "0.5.1";
  };
  drv = { mkDerivation, base, bytestring, data-default-class, deepseq
, HUnit, lib, QuickCheck, tasty, tasty-hunit, tasty-quickcheck
, text, time
}:
mkDerivation {
  pname = "cookie";
  version = "0.5.1";
  src = /nix/store/dfxfs6w045dqncak1n7sgapzscg62y6d-source;
  libraryHaskellDepends = [
    base bytestring data-default-class deepseq text time
  ];
  testHaskellDepends = [
    base bytestring HUnit QuickCheck tasty tasty-hunit tasty-quickcheck
    text time
  ];
  homepage = "https://github.com/snoyberg/cookie";
  description = "HTTP cookie parsing and rendering";
  license = lib.licenses.mit;
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
  data-default = {
  meta = {
    sha256 = "0j38fzp7rrd6rxl7pbxnbamlgbx07kb240rcmpnlq9i62fw5zvpx";
    url = "https://hackage.haskell.org";
    ver = "0.7.1.3";
  };
  drv = { mkDerivation, base, containers, data-default-class
, data-default-instances-containers, data-default-instances-dlist
, data-default-instances-old-locale, lib, mtl, old-locale
}:
mkDerivation {
  pname = "data-default";
  version = "0.7.1.3";
  src = /nix/store/x5s5fz735wvvglpk9rk80ks49iv5w3wy-source;
  libraryHaskellDepends = [
    base data-default-class data-default-instances-containers
    data-default-instances-dlist data-default-instances-old-locale
  ];
  testHaskellDepends = [ base containers mtl old-locale ];
  description = "A class for types with a default value";
  license = lib.licenses.bsd3;
}
;
}
;
  data-default-class = {
  meta = {
    sha256 = "1jw6s5ny8bv767fqmzn30dcvhlc3qidsqsq7vaxzknwm034683rr";
    url = "https://hackage.haskell.org";
    ver = "0.1.2.2";
  };
  drv = { mkDerivation, base, lib }:
mkDerivation {
  pname = "data-default-class";
  version = "0.1.2.2";
  src = /nix/store/51pi1z7963q31n3svdsmpbls874af0h0-source;
  libraryHaskellDepends = [ base ];
  description = "A class for types with a default value";
  license = lib.licenses.bsd3;
}
;
}
;
  data-default-instances-containers = {
  meta = {
    sha256 = "1fhk69ydxbzvf2xr1jsp87z92zq1r7p0hnpigihndvl2mghv5snm";
    url = "https://hackage.haskell.org";
    ver = "0.1.0.3";
  };
  drv = { mkDerivation, base, containers, data-default-class, lib }:
mkDerivation {
  pname = "data-default-instances-containers";
  version = "0.1.0.3";
  src = /nix/store/dwan2g7w432jgwxyl4ciph3i5137rjxb-source;
  libraryHaskellDepends = [ base containers data-default-class ];
  description = "Default instances for types in containers";
  license = lib.licenses.bsd3;
}
;
}
;
  data-default-instances-dlist = {
  meta = {
    sha256 = "1db6fas45p4z4bd819fqin4x58aikahwh3h3ri3c9hql48dfkmy5";
    url = "https://hackage.haskell.org";
    ver = "0.0.1.2";
  };
  drv = { mkDerivation, base, data-default-class, dlist, lib }:
mkDerivation {
  pname = "data-default-instances-dlist";
  version = "0.0.1.2";
  src = /nix/store/bqy320ibj1shl0l3cm7cnz71qjsr7zjf-source;
  libraryHaskellDepends = [ base data-default-class dlist ];
  description = "Default instances for types in dlist";
  license = lib.licenses.bsd3;
}
;
}
;
  data-default-instances-old-locale = {
  meta = {
    sha256 = "17k5w0n6zxjrffwpws8gvaz5r7yilhzd2b4yh80ihv9jkmil9nyd";
    url = "https://hackage.haskell.org";
    ver = "0.0.1.2";
  };
  drv = { mkDerivation, base, data-default-class, lib, old-locale }:
mkDerivation {
  pname = "data-default-instances-old-locale";
  version = "0.0.1.2";
  src = /nix/store/2kizhaqpfy9s1960mdmjbg9ww04l88rr-source;
  libraryHaskellDepends = [ base data-default-class old-locale ];
  description = "Default instances for types in old-locale";
  license = lib.licenses.bsd3;
}
;
}
;
  exon = {
  meta = {
    sha256 = "142i8ka6b16ydllhhb2305ml3hij66h6y555fp6cvc82166kdrhb";
    url = "https://hackage.haskell.org";
    ver = "1.7.0.0";
  };
  drv = { mkDerivation, base, criterion, ghc, hedgehog, incipit-base, lib
, parsec, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "exon";
  version = "1.7.0.0";
  src = /nix/store/p3qg36znxvgfmg2vj3gbcsq1m92l6j2s-source;
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
  hedgehog = {
  meta = {
    sha256 = "1qxxhs720im0wpa5lsca0l8qsfmhbyphd1aq01nv96v29lgv795b";
    url = "https://hackage.haskell.org";
    ver = "1.4";
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
  version = "1.4";
  src = /nix/store/h0hfs9fnv1wpvc4x48m9i5p66gx0li8w-source;
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
    sha256 = "1ddx0x74kgqxa84fv5m9k7c5pg34n6b7snrj9kss3ahc4k3p8s1l";
    url = "https://hackage.haskell.org";
    ver = "0.7.14";
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
  version = "0.7.14";
  src = /nix/store/f2awq20ndbl7nnb9x7pl8z53idq02yvk-source;
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
    sha256 = "108ljq87pdvr2fg8140a1vs6qvyhd3fgzsaml644svif76mpkhif";
    url = "https://hackage.haskell.org";
    ver = "0.12";
  };
  drv = { mkDerivation, array, base, bytestring, case-insensitive, doctest
, hspec, lib, QuickCheck, quickcheck-instances, text
}:
mkDerivation {
  pname = "http-types";
  version = "0.12";
  src = /nix/store/896wqw6xr03lcnvm7v1zszs6n7bd5i33-source;
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
  incipit-base = {
  meta = {
    sha256 = "1hck35yfy0dcgimgnd90w02zvv7x7k456bljrbx2mwxalnhav9gf";
    url = "https://hackage.haskell.org";
    ver = "0.6.0.0";
  };
  drv = { mkDerivation, base, bytestring, containers, data-default, lib
, stm, text
}:
mkDerivation {
  pname = "incipit-base";
  version = "0.6.0.0";
  src = /nix/store/bcs2wgdcfmnm1msbd7n8qd27ikwv3rcm-source;
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
    sha256 = "1h1c9a2a553h8wjpx7szk60nh5ag6qqbxj7sl9jbm5hvpxhaq75m";
    url = "https://hackage.haskell.org";
    ver = "5.3.2";
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
  version = "5.3.2";
  src = /nix/store/ph8g2zjk5w7rdh4wwci23gjyd1gm5dzn-source;
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
    sha256 = "1ppxml1hd6nbsa2ghxg6b199wvj7v178mkydg48b3gd9z39rg543";
    url = "https://hackage.haskell.org";
    ver = "1.1.0.0";
  };
  drv = { mkDerivation, base, bytestring, containers, gauge, hspec, lens
, lib, pcre-heavy, pcre-light, template-haskell, text
}:
mkDerivation {
  pname = "lens-regex-pcre";
  version = "1.1.0.0";
  src = /nix/store/mz3c9h8fs592b91aa3qicmr7vnjjgap8-source;
  libraryHaskellDepends = [
    base bytestring containers lens pcre-heavy pcre-light
    template-haskell text
  ];
  testHaskellDepends = [
    base bytestring containers hspec lens pcre-heavy pcre-light
    template-haskell text
  ];
  benchmarkHaskellDepends = [
    base bytestring containers gauge lens pcre-heavy pcre-light
    template-haskell text
  ];
  homepage = "https://github.com/ChrisPenner/lens-regex-pcre#readme";
  description = "A lensy interface to regular expressions";
  license = lib.licenses.bsd3;
}
;
}
;
  path-io = {
  meta = {
    sha256 = "134nc4dnslvad6vsmlyjvp1l4y9qsnq7wnnlm79s8c27nwajj8dm";
    url = "https://hackage.haskell.org";
    ver = "1.8.0";
  };
  drv = { mkDerivation, base, containers, directory, dlist, exceptions
, filepath, hspec, lib, path, temporary, time, transformers
, unix-compat
}:
mkDerivation {
  pname = "path-io";
  version = "1.8.0";
  src = /nix/store/9y1vmk88qjh8ik56mgv3kmsjs2zn0dy4-source;
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
  socks = {
  meta = {
    sha256 = "0750g9cd22w0hlxcz8hrghlrbgsyja3x994d9m7sq5jh3s52da92";
    url = "https://hackage.haskell.org";
    ver = "0.6.1";
  };
  drv = { mkDerivation, base, basement, bytestring, cereal, lib, network }:
mkDerivation {
  pname = "socks";
  version = "0.6.1";
  src = /nix/store/2z1xp1krgp2a86jvjcjcziqwc00g19i7-source;
  libraryHaskellDepends = [
    base basement bytestring cereal network
  ];
  homepage = "http://github.com/vincenthz/hs-socks";
  description = "Socks proxy (ver 5)";
  license = lib.licenses.bsd3;
}
;
}
;
  tasty = {
  meta = {
    sha256 = "1jqrcmibqv03109qc6lhi2jnip4mxygcd0j4j0g1n0q0akcplica";
    url = "https://hackage.haskell.org";
    ver = "1.5.2";
  };
  drv = { mkDerivation, ansi-terminal, base, containers, lib
, optparse-applicative, stm, tagged, transformers, unix
}:
mkDerivation {
  pname = "tasty";
  version = "1.5.2";
  src = /nix/store/ly5d0sscd0dwlyr06nqhyscj3a99j8np-source;
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