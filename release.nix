{}:

let
  ## ---------------------------------------------------------------------------
  ## -- Nixpkgs configuration

  haskell = import ./nix/haskell.nix { compiler = "ghc821"; };
  pkgconf =
    { allowUnfree = true;
      inherit (haskell) packageOverrides;
    };

  nixpkgs = (import ./nix/nixpkgs.nix) { config = pkgconf; };

  fpga = import ./nix/fpga.nix { inherit nixpkgs; };

  ## ---------------------------------------------------------------------------
  ## -- Shell environment

  haskellEnv = haskell.makeEnv nixpkgs
    (haskellPackages: with haskellPackages; [
      shake
      ghcid

      haskellPackages.ghc
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
    name = "de10-nano-interactive";
    src  = "./";

    # Ordinary, non-overridden packages
    pkgInputs = with nixpkgs;
      [ z3 cvc4 yices
        yosys arachne-pnr icestorm

        verilog ghdl_llvm

        # Utilities
        nix-prefetch-git nix-prefetch-hg
        cabal-install cabal2nix

        silver-searcher
      ];

    # Overridden packages
    myInputs = with fpga;
      [ symbiyosys suprove
        haskellEnv
      ];

    buildInputs = pkgInputs ++ myInputs;
    in nixpkgs.stdenv.mkDerivation { inherit name src buildInputs; };

## ---------------------------------------------------------------------------
  ## -- Main export for release.nix

  self = rec
    { inherit shell;
    };
in self
