{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
     owner = "NixOS";
     repo = "nixpkgs";
     rev = "b9fa31c";
     sha256 = "1iqdra7nvcwbydjirjsk71rpzk4ljc0gzqy33fcp8l18y8iwh47k";
   }){},
}:

let
  ghcjs = pkgs.haskell.packages.ghcjs84.override {
     overrides = self: super: {
       aeson = pkgs.haskell.lib.dontCheck super.aeson;
       http-types = pkgs.haskell.lib.dontCheck super.http-types;
       natural-transformation = pkgs.haskell.lib.dontCheck super.natural-transformation;
       scientific = pkgs.haskell.lib.dontCheck super.scientific;
       servant = pkgs.haskell.lib.dontCheck super.servant;
       uri-bytestring = pkgs.haskell.lib.dontCheck super.uri-bytestring;
       uuid-types = pkgs.haskell.lib.dontCheck super.uuid-types;
       foundation = pkgs.haskell.lib.dontCheck super.foundation;
     };
  };

  result = import (pkgs.fetchFromGitHub {
    owner = "puffnfresh";
    repo = "miso";
    sha256 = "1nng214balji3r1f0afh75vkmip1xn66ml8fmg899f6v71yqi307";
    rev = "1c29116cdc8454337097aee23cbf0bd6744c1d41";
  }) {};

  miso-ghcjs = result.miso-ghcjs;

in
  ghcjs.callCabal2nix "miso-plane" ./. {
    miso = miso-ghcjs;
  }