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
    sha256 = "0b3jm5a17r8dhxkzq2xk09rcr72x0hk9zggcidn3zigqpcyqdhzz";
    rev = "f2f9b2fc405354af653bf65f4ac2e38b6c33aa99";
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
