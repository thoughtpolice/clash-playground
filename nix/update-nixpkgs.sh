#!/usr/bin/env bash
set -e

if [[ "x$1" == "x" ]]; then
    echo "Must provide a revision argument"
    echo "Usage:"
    echo "  ./update-nixpkgs.sh <rev>"
    echo "  ./update-nixpkgs.sh https://github.com/foo/nixpkgs <rev>"
    exit 1
fi

if [[ "x$2" == "x" ]]; then
    REV="$1"
    URL="https://github.com/nixos/nixpkgs"
else
    REV="$2"
    URL="$1"
fi

DOWNLOAD="$URL/archive/$REV.tar.gz"
echo "Updating to nixpkgs revision $REV from $URL"
SHA256=$(nix-prefetch-url "$DOWNLOAD")

cat > nixpkgs.json <<EOF
{
  "url":    "$DOWNLOAD",
  "rev":    "$REV",
  "sha256": "$SHA256"
}
EOF

echo "Updated nixpkgs.json"
