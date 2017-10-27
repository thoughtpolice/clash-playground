{ mkDerivation, array, base, bifunctors, constraints, criterion
, data-binary-ieee754, data-default, deepseq, doctest, fetchgit
, ghc-prim, ghc-typelits-extra, ghc-typelits-knownnat
, ghc-typelits-natnormalise, half, integer-gmp, lens, QuickCheck
, reflection, singletons, stdenv, template-haskell, transformers
, vector
}:
mkDerivation {
  pname = "clash-prelude";
  version = "0.99";
  src = fetchgit {
    url = "https://github.com/clash-lang/clash-prelude.git";
    sha256 = "0xws6mhwv8ipm5j1awly9am0kyvwagb8hv95i2f52lyq28w4wvkc";
    rev = "f095469f29b072e88b3b1ce47f857e9f657909b9";
  };
  libraryHaskellDepends = [
    array base bifunctors constraints data-binary-ieee754 data-default
    deepseq ghc-prim ghc-typelits-extra ghc-typelits-knownnat
    ghc-typelits-natnormalise half integer-gmp lens QuickCheck
    reflection singletons template-haskell transformers vector
  ];
  testHaskellDepends = [ base doctest ];
  benchmarkHaskellDepends = [
    base criterion deepseq template-haskell
  ];
  homepage = "http://www.clash-lang.org/";
  description = "CAES Language for Synchronous Hardware - Prelude library";
  license = stdenv.lib.licenses.bsd2;
}
