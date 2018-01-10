{ mkDerivation, base, fetchgit, ghc, ghc-tcplugins-extra
, ghc-typelits-natnormalise, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, transformers
}:
mkDerivation {
  pname = "ghc-typelits-knownnat";
  version = "0.4";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-typelits-knownnat.git";
    sha256 = "0qlfizflyz9xadjaib5nckgzbwb8bk07npaqlvz4gwiwzsasq9j5";
    rev = "4f8c1f833baf6354ea41e9b634e0beed53ab3475";
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
