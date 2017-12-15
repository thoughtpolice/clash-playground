{ mkDerivation, aeson, attoparsec, base, bytestring, clash-prelude
, concurrent-supply, containers, data-binary-ieee754, deepseq
, directory, errors, fetchgit, fgl, filepath, ghc, hashable
, integer-gmp, lens, mtl, pretty, process, stdenv, template-haskell
, text, time, transformers, unbound-generics, unordered-containers
, uu-parsinglib, wl-pprint-text
}:
mkDerivation {
  pname = "clash-lib";
  version = "0.99";
  src = fetchgit {
    url = "https://github.com/clash-lang/clash-compiler.git";
    sha256 = "0b3jm5a17r8dhxkzq2xk09rcr72x0hk9zggcidn3zigqpcyqdhzz";
    rev = "f2f9b2fc405354af653bf65f4ac2e38b6c33aa99";
  };
  postUnpack = "sourceRoot+=/clash-lib; echo source root reset to $sourceRoot";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring clash-prelude concurrent-supply
    containers data-binary-ieee754 deepseq directory errors fgl
    filepath ghc hashable integer-gmp lens mtl pretty process
    template-haskell text time transformers unbound-generics
    unordered-containers uu-parsinglib wl-pprint-text
  ];
  homepage = "http://www.clash-lang.org/";
  description = "CAES Language for Synchronous Hardware - As a Library";
  license = stdenv.lib.licenses.bsd2;
}
