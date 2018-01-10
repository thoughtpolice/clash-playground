{ mkDerivation, base, fetchgit, ghc, ghc-prim, ghc-tcplugins-extra
, ghc-typelits-knownnat, ghc-typelits-natnormalise, integer-gmp
, stdenv, tasty, tasty-hunit, template-haskell, transformers
}:
mkDerivation {
  pname = "ghc-typelits-extra";
  version = "0.2.4";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-typelits-extra.git";
    sha256 = "0hchwbi1wjswm59gll2ivdfpi2mf6hbnf7lygzp0j9mbf4pciqrq";
    rev = "08bed714eaf52bfc46ac1222bed38c33a7c9354c";
  };
  libraryHaskellDepends = [
    base ghc ghc-prim ghc-tcplugins-extra ghc-typelits-knownnat
    ghc-typelits-natnormalise integer-gmp transformers
  ];
  testHaskellDepends = [
    base ghc-typelits-knownnat ghc-typelits-natnormalise tasty
    tasty-hunit template-haskell
  ];
  homepage = "http://www.clash-lang.org/";
  description = "Additional type-level operations on GHC.TypeLits.Nat";
  license = stdenv.lib.licenses.bsd2;
}
