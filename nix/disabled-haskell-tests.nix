/*

These are overrides for Haskell packages that are marked as broken in nixpkgs
solely because their tests fail. The test failures are typically just because
the tests don't work in a pure nix environment, and so here we override them by
skipping the tests and marking the package as not broken.

*/
{ nixpkgs }:
let
  list = [
    # list of string package names, currently empty
  ];
  inherit (builtins) map listToAttrs;
  inherit (nixpkgs.stable.haskell.lib) overrideCabal;
in

self: super:
listToAttrs (
  map
    (name: {
      inherit name;
      value = overrideCabal super.${name}
        (drv: { doCheck = false; broken = false; });
    })
    list
)
