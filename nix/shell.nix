{ nixpkgs, system }:
nixpkgs.stable.mkShell {
  name = "libsystemd-journal-shell";
  buildInputs =
    with (import ./. { inherit nixpkgs; });
    [
      ghc-with-packages
      nixpkgs.stable.cabal-install
      nixpkgs.stable.pkg-config-unwrapped
    ];
  shellHook = ''
    PKG_CONFIG_PATH+=":${nixpkgs.stable.systemd.dev}/lib/pkgconfig"
    export PKG_CONFIG_PATH
  '';
}
