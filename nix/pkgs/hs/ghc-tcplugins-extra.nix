{ mkDerivation, base, fetchgit, ghc, stdenv }:
mkDerivation {
  pname = "ghc-tcplugins-extra";
  version = "0.3";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-tcplugins-extra";
    sha256 = "0ghfndlzw3rdnyxyxjgdbmcnkk985x65wga00ky1acxhlq6md4n4";
    rev = "ac70960df5b04ec092ea189c8d34b28ab9b41695";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base ghc ];
  homepage = "http://github.com/clash-lang/ghc-tcplugins-extra";
  description = "Utilities for writing GHC type-checker plugins";
  license = stdenv.lib.licenses.bsd2;
}
