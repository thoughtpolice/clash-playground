{ mkDerivation, base, fetchgit, ghc, stdenv }:
mkDerivation {
  pname = "ghc-tcplugins-extra";
  version = "0.2.2";
  src = fetchgit {
    url = "https://github.com/clash-lang/ghc-tcplugins-extra.git";
    sha256 = "1pvx31gkzj15lfpx68palzfgssli2hiibgqaydq1ny8lb6ia1076";
    rev = "5acf45e24e15f096a07dea6be0163fb1a465d4fa";
  };
  libraryHaskellDepends = [ base ghc ];
  homepage = "http://github.com/clash-lang/ghc-tcplugins-extra";
  description = "Utilities for writing GHC type-checker plugins";
  license = stdenv.lib.licenses.bsd2;
}
