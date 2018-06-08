{ mkDerivation, aeson, ansi-wl-pprint, attoparsec, base, bytestring
, clash-prelude, concurrent-supply, containers, data-binary-ieee754
, deepseq, directory, errors, fetchgit, fgl, filepath, ghc
, hashable, integer-gmp, lens, mtl, parsers, prettyprinter, process
, reducers, stdenv, template-haskell, text, time, transformers
, trifecta, unbound-generics, unordered-containers
}:
mkDerivation {
  pname = "clash-lib";
  version = "0.99";
  src = fetchgit {
    url = "https://github.com/clash-lang/clash-compiler";
    sha256 = "1wyxw2x4g3ynbw7ys211i1j52vv78fvc765m98g2m2z8pbxsa1r5";
    rev = "e1bc893ada6ac17cfe4c4622f1b7d80f6745688e";
  };
  postUnpack = "sourceRoot+=/clash-lib; echo source root reset to $sourceRoot";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint attoparsec base bytestring clash-prelude
    concurrent-supply containers data-binary-ieee754 deepseq directory
    errors fgl filepath ghc hashable integer-gmp lens mtl parsers
    prettyprinter process reducers template-haskell text time
    transformers trifecta unbound-generics unordered-containers
  ];
  homepage = "http://www.clash-lang.org/";
  description = "CAES Language for Synchronous Hardware - As a Library";
  license = stdenv.lib.licenses.bsd2;
}
