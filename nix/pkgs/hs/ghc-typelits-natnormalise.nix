{ mkDerivation, base, fetchgit, ghc, ghc-tcplugins-extra
, integer-gmp, stdenv, tasty, tasty-hunit, template-haskell
, transformers
}:
mkDerivation {
  pname = "ghc-typelits-natnormalise";
  version = "0.6.2";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-typelits-natnormalise";
    sha256 = "1p1gnn4cz266f51r7vg00zi65251l7pkkxlrd8804b6y7n5w5w23";
    rev = "4994e39123f53678916ebea81f6f22421f17577a";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc ghc-tcplugins-extra integer-gmp transformers
  ];
  testHaskellDepends = [ base tasty tasty-hunit template-haskell ];
  homepage = "http://www.clash-lang.org/";
  description = "GHC typechecker plugin for types of kind GHC.TypeLits.Nat";
  license = stdenv.lib.licenses.bsd2;
}
