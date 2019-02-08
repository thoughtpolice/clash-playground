{ mkDerivation, array, base, bifunctors, bytestring, clash-lib
, clash-prelude, concurrent-supply, containers, deepseq, directory
, fetchgit, filepath, ghc, ghc-boot, ghc-prim, ghc-typelits-extra
, ghc-typelits-knownnat, ghc-typelits-natnormalise, ghci, hashable
, haskeline, integer-gmp, lens, mtl, primitive, process, reflection
, stdenv, template-haskell, text, time, transformers, uniplate
, unix, unordered-containers, vector
}:
mkDerivation {
  pname = "clash-ghc";
  version = "0.99";
  src = fetchgit {
    url = "https://github.com/clash-lang/clash-compiler";
    sha256 = "1bvwwlppmghs8hm7c74ipkhd5drn048895ywj6p85x07052a7im5";
    rev = "a2c4dc3ca60be4fa094697ae98de45e4df5ce03e";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/clash-ghc; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bifunctors bytestring clash-lib clash-prelude
    concurrent-supply containers deepseq directory filepath ghc
    ghc-boot ghc-prim ghc-typelits-extra ghc-typelits-knownnat
    ghc-typelits-natnormalise ghci hashable haskeline integer-gmp lens
    mtl primitive process reflection template-haskell text time
    transformers uniplate unix unordered-containers vector
  ];
  executableHaskellDepends = [ base ];
  homepage = "http://www.clash-lang.org/";
  description = "CAES Language for Synchronous Hardware";
  license = stdenv.lib.licenses.bsd2;
}
