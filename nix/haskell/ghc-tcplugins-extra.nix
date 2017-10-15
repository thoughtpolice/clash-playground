{ mkDerivation, base, fetchgit, ghc, stdenv }:
mkDerivation {
  pname = "ghc-tcplugins-extra";
  version = "0.2.1";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-tcplugins-extra.git";
    sha256 = "0sc3i7cj0cqaikkdxb8qcw40w799y4wqazffmdn01dhd7z83ihn3";
    rev = "e70869d3ec9922166e21753421ca4bec0ecf49c2";
  };
  libraryHaskellDepends = [ base ghc ];
  homepage = "http://github.com/clash-lang/ghc-tcplugins-extra";
  description = "Utilities for writing GHC type-checker plugins";
  license = stdenv.lib.licenses.bsd2;
}
