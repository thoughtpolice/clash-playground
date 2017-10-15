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
    url = "https://github.com/clash-lang/clash-compiler";
    sha256 = "0zcpd9phw183vrb52ig3njxniri6sw3zbn4wwi3099hyi9kxcl0g";
    rev = "698a2a1f0c4f4dfe29f7e66116cc0a56ea475938";
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
