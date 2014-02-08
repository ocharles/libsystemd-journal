{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall_1_18_0_2
    hsyslog pipes pipesSafe text uuid uniplate unixBytestring unorderedContainers vector;

  pkgs = import <nixpkgs> {};

in cabal.mkDerivation (self: {
  pname = "libsystemd-journal";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ hsyslog pipes pipesSafe text uuid unixBytestring uniplate unorderedContainers vector ];
  extraLibraries = [ pkgs.systemd ];
  buildTools = [ cabalInstall_1_18_0_2 ];
  enableSplitObjs = false;
})