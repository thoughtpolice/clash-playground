{ mkDerivation, base, Cabal, containers, directory, fetchgit
, filepath, mtl, process, stdenv, syb, tasty, tasty-golden
, template-haskell, text, th-desugar
}:
mkDerivation {
  pname = "singletons";
  version = "2.3.1";
  src = fetchgit {
    url = "https://github.com/goldfirere/singletons.git";
    sha256 = "106iw4dsrgk6zsf49kbsiy3dg5q193bxihh3azxgf8gy48ymagck";
    rev = "d0fdb2cf02f29d6d076354696aaceb57f2715c85";
  };
  libraryHaskellDepends = [
    base containers mtl syb template-haskell text th-desugar
  ];
  testHaskellDepends = [
    base Cabal directory filepath process tasty tasty-golden
  ];
  homepage = "http://www.github.com/goldfirere/singletons";
  description = "A framework for generating singleton types";
  license = stdenv.lib.licenses.bsd3;
}
