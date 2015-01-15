with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, bytestring, hashable, hsyslog, systemd
             , pipes, pipes-safe, stdenv, text, transformers, uniplate
             , unix-bytestring, unordered-containers, uuid, vector
             }:
             mkDerivation {
               pname = "libsystemd-journal";
               version = "1.3.1";
               src = ./.;
               buildDepends = [
                 base bytestring hashable hsyslog pipes pipes-safe text transformers
                 uniplate unix-bytestring unordered-containers uuid vector
               ];
               pkgconfigDepends = [ systemd ];
               homepage = "http://github.com/ocharles/libsystemd-journal";
               description = "Haskell bindings to libsystemd-journal";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
