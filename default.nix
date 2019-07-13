{ nixpkgs ? (import <nixpkgs> {}).fetchFromGitHub {
     owner = "NixOS";
     repo = "nixpkgs";
     rev = "4dd5c93998da55002fdec1c715c680531420381c";
     sha256 = "06paxakic36nbdnwkkb1094fzp3lpzxxb1r57gmb3py6pb6xrcnh";
   }
}:

let
  overrides = pkgs: {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc864 = pkgs.haskell.packages.ghc864.override {
          overrides = self: super: with pkgs.haskell.lib; {
            happy = dontCheck (super.callHackage "happy" "1.19.9" {});
            mkDerivation = args: super.mkDerivation (args // {
              enableLibraryProfiling = false;
              doCheck = false;
              doHaddock = false;
            });
          };
        };
        ghcjs86 = pkgs.haskell.packages.ghcjs86.override {
          overrides = self: super: {
            jsaddle-warp = super.callPackage ./jsaddle-warp-ghcjs.nix {};
            mkDerivation = args: super.mkDerivation (args // { doCheck = false; });
            doctest = null;
          };
        };
      };
    };
  };

  pkgs = import nixpkgs { config.packageOverrides = overrides; };

  ghcjs = pkgs.haskell.packages.ghcjs;

  miso = ghcjs.callCabal2nix "miso" (pkgs.fetchFromGitHub {
    owner  = "dmjio";
    repo   = "miso";
    rev    = "9b05dbb25feae1b1dc9ac0b7da2c1fc875374c1d";
    sha256 = "1h46mmlcszm4nmwq7yiy3sdks0xy3ww3pm41cz6zpg1hmikf40fm";
  }){};

in
  ghcjs.callCabal2nix "miso-plane" ./. {
    inherit miso;
  }
