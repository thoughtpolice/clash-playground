{ mkDerivation, base, fetchgit, ghc, stdenv }:
mkDerivation {
  pname = "ghc-tcplugins-extra";
  version = "0.3";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-tcplugins-extra.git";
    sha256 = "1f5c0pixg9vwsq6ym662f74f7pl8s4im4k3ndqgwd10zzkdx4ibr";
    rev = "4f2defb334da089100a16c8e75ba462b06c9d465";
  };
  libraryHaskellDepends = [ base ghc ];
  homepage = "http://github.com/clash-lang/ghc-tcplugins-extra";
  description = "Utilities for writing GHC type-checker plugins";
  license = stdenv.lib.licenses.bsd2;
}
