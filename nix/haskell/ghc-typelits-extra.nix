{ mkDerivation, base, fetchgit, ghc, ghc-prim, ghc-tcplugins-extra
, ghc-typelits-knownnat, ghc-typelits-natnormalise, integer-gmp
, singletons, stdenv, tasty, tasty-hunit, template-haskell
, transformers
}:
mkDerivation {
  pname = "ghc-typelits-extra";
  version = "0.2.3";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-typelits-extra.git";
    sha256 = "0gwzvi7ghkzspcq3ad70zqxvwk07gnxbi36z52sd1jrm8ps983pd";
    rev = "6ceaae7991647924084f5bab40a08eec2c9933e5";
  };
  libraryHaskellDepends = [
    base ghc ghc-prim ghc-tcplugins-extra ghc-typelits-knownnat
    ghc-typelits-natnormalise integer-gmp singletons transformers
  ];
  testHaskellDepends = [
    base ghc-typelits-knownnat ghc-typelits-natnormalise tasty
    tasty-hunit template-haskell
  ];
  homepage = "http://www.clash-lang.org/";
  description = "Additional type-level operations on GHC.TypeLits.Nat";
  license = stdenv.lib.licenses.bsd2;
}
