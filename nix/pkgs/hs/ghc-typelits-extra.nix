{ mkDerivation, base, fetchgit, ghc, ghc-prim, ghc-tcplugins-extra
, ghc-typelits-knownnat, ghc-typelits-natnormalise, integer-gmp
, stdenv, tasty, tasty-hunit, template-haskell, transformers
}:
mkDerivation {
  pname = "ghc-typelits-extra";
  version = "0.3";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-typelits-extra";
    sha256 = "159z5k68yiri75zxp0fxb82clna7c57wll2fwwm17vfhba3780hh";
    rev = "f1cba7cebf73e429dbdfa67c88161300bc5e318e";
    fetchSubmodules = true;
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
