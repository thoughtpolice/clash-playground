{ mkDerivation, base, fetchgit, ghc, ghc-tcplugins-extra
, integer-gmp, stdenv, tasty, tasty-hunit, template-haskell
}:
mkDerivation {
  pname = "ghc-typelits-natnormalise";
  version = "0.5.3";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-typelits-natnormalise.git";
    sha256 = "1mq0pfh27il21znsbiacwl2gxj1hnag7vvscp7hdvswp5r3r9zz4";
    rev = "a1fb2a0f177ddf6f216076f8cf090191a2897789";
  };
  libraryHaskellDepends = [
    base ghc ghc-tcplugins-extra integer-gmp
  ];
  testHaskellDepends = [ base tasty tasty-hunit template-haskell ];
  homepage = "http://www.clash-lang.org/";
  description = "GHC typechecker plugin for types of kind GHC.TypeLits.Nat";
  license = stdenv.lib.licenses.bsd2;
}
