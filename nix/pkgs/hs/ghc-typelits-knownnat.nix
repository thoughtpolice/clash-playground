{ mkDerivation, base, fetchgit, ghc, ghc-tcplugins-extra
, ghc-typelits-natnormalise, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, transformers
}:
mkDerivation {
  pname = "ghc-typelits-knownnat";
  version = "0.5";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-typelits-knownnat.git";
    sha256 = "191n5flwmrz3vf7p384kanw6kfvpgbiy77sr3jccfbq4f86g2lsj";
    rev = "c3a4c99ec8307393887790b9080ecdc8fe4ae51a";
  };
  libraryHaskellDepends = [
    base ghc ghc-tcplugins-extra ghc-typelits-natnormalise
    template-haskell transformers
  ];
  testHaskellDepends = [
    base ghc-typelits-natnormalise tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "http://clash-lang.org/";
  description = "Derive KnownNat constraints from other KnownNat constraints";
  license = stdenv.lib.licenses.bsd2;
}
