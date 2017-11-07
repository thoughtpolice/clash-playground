{ config }:

let
  nixpkgs   = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  src       = import ./fetchnix.nix { inherit (nixpkgs) rev sha256; };
in import src { inherit config; }
