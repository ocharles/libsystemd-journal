/*

This package contains GHC and a bunch of Haskell packages.

*/
{ nixpkgs }:
let
  haskellPackages = nixpkgs.stable.haskellPackages.override {
    inherit (nixpkgs.unstable) all-cabal-hashes;
    overrides = import ./haskell-package-overrides.nix
      { inherit nixpkgs; };
  };
in
haskellPackages.ghcWithPackages (import ./haskell-package-selection.nix)
