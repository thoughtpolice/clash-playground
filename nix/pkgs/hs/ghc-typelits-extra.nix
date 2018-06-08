{ mkDerivation, base, fetchgit, ghc, ghc-prim, ghc-tcplugins-extra
, ghc-typelits-knownnat, ghc-typelits-natnormalise, integer-gmp
, stdenv, tasty, tasty-hunit, template-haskell, transformers
}:
mkDerivation {
  pname = "ghc-typelits-extra";
  version = "0.2.5";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-typelits-extra.git";
    sha256 = "0n2rwfrm31bndnl3cg5m9gh0b2ha12nmsv0hrd1681x7aipganim";
    rev = "21dafece0a166134e4f5a3b1bcbc2fb0f42ab90b";
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
