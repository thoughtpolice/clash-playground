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
    sha256 = "0wdalrvdycmvfrgnv6w19hwymm70j1722vzwyrii1yyaxagd3dxh";
    rev = "3f5e43bb5c238876e111c8ea0d6eecb910b09ed4";
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
