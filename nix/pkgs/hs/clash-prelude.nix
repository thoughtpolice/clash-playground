{ mkDerivation, array, base, bifunctors, constraints, containers
, criterion, data-binary-ieee754, data-default-class, deepseq
, doctest, fetchgit, ghc-prim, ghc-typelits-extra
, ghc-typelits-knownnat, ghc-typelits-natnormalise, half, hashable
, integer-gmp, lens, QuickCheck, reflection, singletons, stdenv
, tasty, tasty-hunit, template-haskell, text, th-lift, th-orphans
, time, transformers, vector
}:
mkDerivation {
  pname = "clash-prelude";
  version = "0.99";
  src = fetchgit {
    url = "https://github.com/clash-lang/clash-compiler";
    sha256 = "1bvwwlppmghs8hm7c74ipkhd5drn048895ywj6p85x07052a7im5";
    rev = "a2c4dc3ca60be4fa094697ae98de45e4df5ce03e";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/clash-prelude; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    array base bifunctors constraints containers data-binary-ieee754
    data-default-class deepseq ghc-prim ghc-typelits-extra
    ghc-typelits-knownnat ghc-typelits-natnormalise half hashable
    integer-gmp lens QuickCheck reflection singletons template-haskell
    text th-lift th-orphans time transformers vector
  ];
  testHaskellDepends = [ base doctest tasty tasty-hunit ];
  benchmarkHaskellDepends = [
    base criterion deepseq template-haskell
  ];
  homepage = "http://www.clash-lang.org/";
  description = "CAES Language for Synchronous Hardware - Prelude library";
  license = stdenv.lib.licenses.bsd2;
}
