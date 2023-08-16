{ nixpkgs }:
{
  ghc-with-packages = import ./ghc-with-packages.nix { inherit nixpkgs; };
}
