{}:

let
  ## ---------------------------------------------------------------------------
  ## -- Nixpkgs configuration

  haskell = import ./nix/pkgs/haskell.nix { compiler = "ghc843"; };
  pkgconf =
    { allowUnfree = true;
      inherit (haskell) packageOverrides;
    };

  nixpkgs = (import ./nix/nixpkgs.nix) { config = pkgconf; };

  ## ---------------------------------------------------------------------------
  ## -- Other non-haskell packages

  fpgaPackages = rec {
    suprove = import ./nix/pkgs/suprove.nix { inherit nixpkgs; };
  };

  ## ---------------------------------------------------------------------------
  ## -- Shell environment

  haskellEnv = haskell.makeEnv nixpkgs
    (haskellPackages: with haskellPackages; [
      haskellPackages.ghc

      shake
      ghcid

      ghc-mtl
      ghc-boot
      ghc-paths

      groom
      lens
      hedgehog
      HUnit
      tasty-hedgehog
      tasty-hunit

      ghc-tcplugins-extra
      ghc-typelits-extra
      ghc-typelits-knownnat
      ghc-typelits-natnormalise
      clash-prelude
      clash-lib
      clash-ghc
    ]);

  shell = let
    name = "clash-playground";
    src  = "./";
    unpackPhase  = ":";
    installPhase = "touch $out";

    # Ordinary, non-overridden packages
    pkgInputs = with nixpkgs;
      [ # RTL/Hardware tools
        symbiyosys yosys
        arachne-pnr icestorm

        # Test/verification
        verilog
        z3 cvc4 yices boolector avy aiger picosat

        # Utilities
        nix-prefetch-git nix-prefetch-hg nix-repl
        cabal-install

        silver-searcher ripgrep
      ];

    # Overridden packages
    myInputs =
      [ fpgaPackages.suprove
        haskellEnv
      ];

    shellHook = with nixpkgs; lib.concatStringsSep "\n"
      [
      ];

    buildInputs = pkgInputs ++ myInputs;
    in nixpkgs.stdenv.mkDerivation {
      inherit name src buildInputs shellHook unpackPhase installPhase;
    };

  ## ---------------------------------------------------------------------------
  ## -- Main export for release.nix

  self = rec
    { inherit shell;
    };
in self
