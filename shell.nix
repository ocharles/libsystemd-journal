{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, hashable, hsyslog, pipes
      , pipes-safe, stdenv, systemd, text, transformers, uniplate
      , unix-bytestring, unordered-containers, uuid, vector
      }:
      mkDerivation {
        pname = "libsystemd-journal";
        version = "1.4.2";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring hashable hsyslog pipes pipes-safe text transformers
          uniplate unix-bytestring unordered-containers uuid vector
        ];
        libraryPkgconfigDepends = [ systemd ];
        homepage = "http://github.com/ocharles/libsystemd-journal";
        description = "Haskell bindings to libsystemd-journal";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f { inherit (pkgs) systemd; });

in

  if pkgs.lib.inNixShell then drv.env else drv
