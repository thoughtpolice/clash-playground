{ mkDerivation, base, fetchgit, ghc, ghc-tcplugins-extra
, integer-gmp, stdenv, tasty, tasty-hunit, template-haskell
}:
mkDerivation {
  pname = "ghc-typelits-natnormalise";
  version = "0.5.8";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-typelits-natnormalise.git";
    sha256 = "1xxmnh97ax4y1zzahcc1zlr6qljgm03ww2jb3ip7z8bwxnnq00ry";
    rev = "4a88b2035cc07f18d7ad1cc6e957596f31b6a235";
  };
  libraryHaskellDepends = [
    base ghc ghc-tcplugins-extra integer-gmp
  ];
  testHaskellDepends = [ base tasty tasty-hunit template-haskell ];
  homepage = "http://www.clash-lang.org/";
  description = "GHC typechecker plugin for types of kind GHC.TypeLits.Nat";
  license = stdenv.lib.licenses.bsd2;
}
