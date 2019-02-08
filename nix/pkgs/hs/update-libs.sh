#! /usr/bin/env nix-shell
#! nix-shell -i bash -p cabal2nix

cabal2nix --subpath clash-prelude https://github.com/clash-lang/clash-compiler > clash-prelude.nix
cabal2nix --subpath clash-ghc https://github.com/clash-lang/clash-compiler > clash-ghc.nix
cabal2nix --subpath clash-lib https://github.com/clash-lang/clash-compiler > clash-lib.nix
cabal2nix https://github.com/clash-lang/ghc-tcplugins-extra > ghc-tcplugins-extra.nix
cabal2nix https://github.com/clash-lang/ghc-typelits-extra > ghc-typelits-extra.nix
cabal2nix https://github.com/clash-lang/ghc-typelits-knownnat > ghc-typelits-knownnat.nix
cabal2nix https://github.com/clash-lang/ghc-typelits-natnormalise > ghc-typelits-natnormalise.nix
