{ mkDerivation, base, fetchgit, ghc, ghc-tcplugins-extra
, integer-gmp, stdenv, tasty, tasty-hunit, template-haskell
, transformers
}:
mkDerivation {
  pname = "ghc-typelits-natnormalise";
  version = "0.6.1";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-typelits-natnormalise.git";
    sha256 = "15n94xi47z519kwj9ms01ynawwi1vcdqx8jq2fhgh7mxi9v51rcm";
    rev = "d4be3b738dded002c5eacedd17ade7671d6eb13b";
  };
  libraryHaskellDepends = [
    base ghc ghc-tcplugins-extra integer-gmp transformers
  ];
  testHaskellDepends = [ base tasty tasty-hunit template-haskell ];
  homepage = "http://www.clash-lang.org/";
  description = "GHC typechecker plugin for types of kind GHC.TypeLits.Nat";
  license = stdenv.lib.licenses.bsd2;
}
