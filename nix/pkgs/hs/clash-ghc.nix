{ mkDerivation, array, base, bifunctors, bytestring, clash-lib
, clash-prelude, concurrent-supply, containers, deepseq, directory
, fetchgit, filepath, ghc, ghc-boot, ghc-prim, ghc-typelits-extra
, ghc-typelits-knownnat, ghc-typelits-natnormalise, ghci, hashable
, haskeline, integer-gmp, lens, mtl, process, reflection, stdenv
, text, time, transformers, unbound-generics, uniplate, unix
, unordered-containers
}:
mkDerivation {
  pname = "clash-ghc";
  version = "0.99";
  src = fetchgit {
    url = "https://github.com/clash-lang/clash-compiler.git";
    sha256 = "0wdalrvdycmvfrgnv6w19hwymm70j1722vzwyrii1yyaxagd3dxh";
    rev = "3f5e43bb5c238876e111c8ea0d6eecb910b09ed4";
  };
  postUnpack = "sourceRoot+=/clash-ghc; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bifunctors bytestring clash-lib clash-prelude
    concurrent-supply containers deepseq directory filepath ghc
    ghc-boot ghc-prim ghc-typelits-extra ghc-typelits-knownnat
    ghc-typelits-natnormalise ghci hashable haskeline integer-gmp lens
    mtl process reflection text time transformers unbound-generics
    uniplate unix unordered-containers
  ];
  executableHaskellDepends = [ base ];
  homepage = "http://www.clash-lang.org/";
  description = "CAES Language for Synchronous Hardware";
  license = stdenv.lib.licenses.bsd2;
}
