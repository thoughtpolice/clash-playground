{ mkDerivation, base, fetchgit, ghc, ghc-tcplugins-extra
, ghc-typelits-natnormalise, singletons, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, transformers
}:
mkDerivation {
  pname = "ghc-typelits-knownnat";
  version = "0.3.1";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-typelits-knownnat.git";
    sha256 = "0q0bdvnxjnq65sv2h0z5b6q5z93pfn9mj4hsbbmp1awimrxp8bbd";
    rev = "804126e306ed5b2c13ee553cd2902a657f968ce4";
  };
  libraryHaskellDepends = [
    base ghc ghc-tcplugins-extra ghc-typelits-natnormalise singletons
    template-haskell transformers
  ];
  testHaskellDepends = [
    base ghc-typelits-natnormalise singletons tasty tasty-hunit
    tasty-quickcheck
  ];
  homepage = "http://clash-lang.org/";
  description = "Derive KnownNat constraints from other KnownNat constraints";
  license = stdenv.lib.licenses.bsd2;
}
