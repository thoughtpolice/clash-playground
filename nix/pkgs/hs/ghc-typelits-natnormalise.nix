{ mkDerivation, base, fetchgit, ghc, ghc-tcplugins-extra
, integer-gmp, stdenv, tasty, tasty-hunit, template-haskell
}:
mkDerivation {
  pname = "ghc-typelits-natnormalise";
  version = "0.5.7";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-typelits-natnormalise.git";
    sha256 = "0czsyvbqy0yjj6dq1bldz40xr8brfcm9ghjprafhvc2s6dv4lm89";
    rev = "d19727ee5a163a61ffd8edd3c7f4a31519f85ea9";
  };
  libraryHaskellDepends = [
    base ghc ghc-tcplugins-extra integer-gmp
  ];
  testHaskellDepends = [ base tasty tasty-hunit template-haskell ];
  homepage = "http://www.clash-lang.org/";
  description = "GHC typechecker plugin for types of kind GHC.TypeLits.Nat";
  license = stdenv.lib.licenses.bsd2;
}
