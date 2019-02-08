{ mkDerivation, base, fetchgit, ghc, ghc-prim, ghc-tcplugins-extra
, ghc-typelits-natnormalise, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, transformers
}:
mkDerivation {
  pname = "ghc-typelits-knownnat";
  version = "0.6";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-typelits-knownnat";
    sha256 = "1s7xf60f9r2i9xhg9p4prm2qw4rvqag0wx1jsrfzrrx8nm3b53rl";
    rev = "7c866bdefff3f8353a29eebb3d35264dacb2af28";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc ghc-prim ghc-tcplugins-extra ghc-typelits-natnormalise
    template-haskell transformers
  ];
  testHaskellDepends = [
    base ghc-typelits-natnormalise tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "http://clash-lang.org/";
  description = "Derive KnownNat constraints from other KnownNat constraints";
  license = stdenv.lib.licenses.bsd2;
}
