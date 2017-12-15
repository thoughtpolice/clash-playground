{ config }:

let
  # Grab the versions we specified in the JSON file
  nixpkgs   = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  # Bootstrap a copy of nixpkgs, based on this.
  src = import ./fetchnix.nix { inherit (nixpkgs) url rev sha256; };

# We use the default nixpkgs configuration during bootstrap.
in import src { inherit config; }
