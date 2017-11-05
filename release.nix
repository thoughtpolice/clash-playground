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
    name = "clash-playground";
    src  = "./";

    # Ordinary, non-overridden packages
    pkgInputs = with nixpkgs;
      [ # RTL/Hardware tools
        symbiyosys yosys
        arachne-pnr icestorm

        # Test/verification
        verilog ghdl_llvm
        z3 cvc4 yices boolector avy aiger picosat

        # Utilities
        nix-prefetch-git nix-prefetch-hg nix-repl
        cabal-install cabal2nix

        silver-searcher ripgrep
      ];

    # Overridden packages
    myInputs = with fpga;
      [ suprove
        haskellEnv
      ];

    shellHook = with nixpkgs; lib.concatStringsSep "\n"
      [
      ];

    buildInputs = pkgInputs ++ myInputs;
    in nixpkgs.stdenv.mkDerivation { inherit name src buildInputs shellHook; };

  ## ---------------------------------------------------------------------------
  ## -- Main export for release.nix

  self = rec
    { inherit shell;
    };
in self
