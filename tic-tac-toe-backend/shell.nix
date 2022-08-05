{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, classy-prelude
      , elm-bridge, hspec, hspec-wai, hspec-wai-json, lib, lucid, mtl
      , random, servant-lucid, servant-server, servant-websockets, text
      , time, wai, wai-app-static, warp, websockets
      }:
      mkDerivation {
        pname = "tic-tac-toe-backend";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring classy-prelude elm-bridge lucid mtl random
          servant-lucid servant-server servant-websockets text time wai
          wai-app-static warp websockets
        ];
        executableHaskellDepends = [
          aeson base bytestring classy-prelude elm-bridge lucid mtl random
          servant-lucid servant-server servant-websockets text time wai
          wai-app-static warp websockets
        ];
        testHaskellDepends = [
          aeson base bytestring classy-prelude hspec hspec-wai hspec-wai-json
          lucid mtl servant-lucid servant-server servant-websockets text time
          wai warp websockets
        ];
        homepage = "https://github.com/githubuser/tic-tac-toe-backend#readme";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
