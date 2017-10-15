{ config }:

let
  bootstrap = import <nixpkgs> {};
  nixpkgs   = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  src       = bootstrap.fetchFromGitHub {
    owner  = "nixos";
    repo   = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };
in import src { inherit config; }
