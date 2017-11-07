{ mkDerivation, base, binary, bytestring, deepseq, directory, extra
, fetchgit, filepath, hashable, js-flot, js-jquery, primitive
, process, QuickCheck, random, stdenv, time, transformers, unix
, unordered-containers, utf8-string
}:
mkDerivation {
  pname = "shake";
  version = "0.16";
  src = fetchgit {
    url = "https://github.com/ndmitchell/shake.git";
    sha256 = "1599mp7fv6rnawnmpnfqwp484xbcyahbbjj2qax8aj2aj5z6l9pw";
    rev = "c4e885f0cdb3b464c58fe86270f4fff256f92b4d";
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base binary bytestring deepseq directory extra filepath hashable
    js-flot js-jquery primitive process random time transformers unix
    unordered-containers utf8-string
  ];
  executableHaskellDepends = [
    base binary bytestring deepseq directory extra filepath hashable
    js-flot js-jquery primitive process random time transformers unix
    unordered-containers utf8-string
  ];
  testHaskellDepends = [
    base binary bytestring deepseq directory extra filepath hashable
    js-flot js-jquery primitive process QuickCheck random time
    transformers unix unordered-containers utf8-string
  ];
  homepage = "http://shakebuild.com";
  description = "Build system library, like Make, but more accurate dependencies";
  license = stdenv.lib.licenses.bsd3;
}
