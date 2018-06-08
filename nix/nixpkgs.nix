{ system ? builtins.currentSystem
, config ? { allowUnfree = true; }
}:

let
  # Grab the versions we specified in the JSON file
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  # Bootstrap a copy of nixpkgs, based on this.
  src = builtins.fetchTarball {
    name = "nixpkgs-${builtins.substring 0 6 nixpkgs.rev}";
    inherit (nixpkgs) url sha256;
  };

in import src { inherit system config; }
