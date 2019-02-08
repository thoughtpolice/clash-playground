{ mkDerivation, aeson, ansi-terminal, ansi-wl-pprint, attoparsec
, base, binary, bytestring, clash-prelude, concurrent-supply
, containers, data-binary-ieee754, deepseq, directory, errors
, exceptions, fetchgit, filepath, ghc, hashable, hint, integer-gmp
, lens, mtl, parsers, prettyprinter, primitive, process, reducers
, stdenv, template-haskell, temporary, text, text-show, time
, transformers, trifecta, unordered-containers, vector
, vector-binary-instances
}:
mkDerivation {
  pname = "clash-lib";
  version = "0.99";
  src = fetchgit {
    url = "https://github.com/clash-lang/clash-compiler";
    sha256 = "1bvwwlppmghs8hm7c74ipkhd5drn048895ywj6p85x07052a7im5";
    rev = "a2c4dc3ca60be4fa094697ae98de45e4df5ce03e";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/clash-lib; echo source root reset to $sourceRoot";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson ansi-terminal ansi-wl-pprint attoparsec base binary
    bytestring clash-prelude concurrent-supply containers
    data-binary-ieee754 deepseq directory errors exceptions filepath
    ghc hashable hint integer-gmp lens mtl parsers prettyprinter
    primitive process reducers template-haskell temporary text
    text-show time transformers trifecta unordered-containers vector
    vector-binary-instances
  ];
  homepage = "http://www.clash-lang.org/";
  description = "CAES Language for Synchronous Hardware - As a Library";
  license = stdenv.lib.licenses.bsd2;
}
