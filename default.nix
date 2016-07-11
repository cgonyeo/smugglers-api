{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, contravariant
      , contravariant-extras, hasql, http-client, http-conduit
      , optparse-applicative, semigroups, servant, servant-client
      , servant-server, stdenv, tagsoup, text, transformers, wai, warp
      }:
      mkDerivation {
        pname = "smugglers-api";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring contravariant contravariant-extras hasql
          http-client http-conduit semigroups servant servant-client
          servant-server tagsoup text transformers wai warp
        ];
        executableHaskellDepends = [
          aeson base bytestring contravariant contravariant-extras hasql
          http-client http-conduit optparse-applicative semigroups servant
          servant-client servant-server tagsoup text transformers wai warp
        ];
        homepage = "https://github.com/dgonyeo/smugglers-api";
        description = "HTTP API that wraps sc.bevager.com";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
