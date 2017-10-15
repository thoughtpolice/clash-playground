{ mkDerivation, base, containers, fetchgit, hspec, HUnit, mtl
, stdenv, syb, template-haskell, th-expand-syns, th-lift
, th-orphans
}:
mkDerivation {
  pname = "th-desugar";
  version = "1.7";
  src = fetchgit {
    url = "https://github.com/goldfirere/th-desugar.git";
    sha256 = "1bn2qx533k4vf1lf329hl8v6kf5n1kar4psb9q3ax7bgs85k5icz";
    rev = "f3f23bec71db8b13544773d0190fc028c1c716d2";
  };
  libraryHaskellDepends = [
    base containers mtl syb template-haskell th-expand-syns th-lift
    th-orphans
  ];
  testHaskellDepends = [
    base containers hspec HUnit mtl syb template-haskell th-expand-syns
    th-lift th-orphans
  ];
  homepage = "https://github.com/goldfirere/th-desugar";
  description = "Functions to desugar Template Haskell";
  license = stdenv.lib.licenses.bsd3;
}
