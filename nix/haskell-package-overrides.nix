{ nixpkgs }:
let
  inherit (nixpkgs.stable.lib) fold composeExtensions;
in
fold composeExtensions (_: _: { }) [
  (self: super: {
    # e.g.
    # asana = super.callHackage "asana" "1.0.1.0" { };
  })
  (import ./disabled-haskell-tests.nix { inherit nixpkgs; })
]
