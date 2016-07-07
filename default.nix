{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, http-conduit
      , servant, servant-server, stdenv, tagsoup, text, transformers, wai
      , warp
      }:
      mkDerivation {
        pname = "smugglers-api";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base bytestring http-conduit servant servant-server tagsoup
          text transformers wai warp
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
