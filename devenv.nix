{ pkgs, inputs, ... }:
let
  hlsPkgs = import inputs.hls-nixpkgs {
    inherit (pkgs.stdenv.hostPlatform) system;
    overlays = [
      (_: prev: {
        haskell = prev.haskell // {
          # Reuse the cached compilers and match the project's GHC ABI exactly.
          compiler = prev.haskell.compiler // {
            ghc9141 = pkgs.haskell.compiler.ghc9141;
            ghc9103 = pkgs.haskell.compiler.ghc9103;
          };
        };
      })
    ];
  };

  # Roc publishes static binaries, so the release tarball is used as-is
  roc = pkgs.stdenvNoCC.mkDerivation rec {
    pname = "roc";
    version = "0.1.0_RC1";
    src = pkgs.fetchurl {
      url = "https://github.com/roc-lang/roc/releases/download/${version}/roc-linux_x86_64-${version}.tar.gz";
      sha256 = "043c426ea85dcdbd427951eaed246f0c10c171d988f2598782233a6dae43379d";
    };
    installPhase = ''
      install -Dm755 roc $out/bin/roc
    '';
  };
in
{
  cachix.pull = [ "haskell-language-server" "nix-community" ];

  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc9141;
    stack.enable = false;
    lsp = {
      enable = true;
      package = hlsPkgs.haskell.packages.ghc9141.haskell-language-server;
    };
  };

  packages = with pkgs; [
    ghciwatch
    just
    fourmolu
    pkg-config
    roc
  ];

  enterTest = "just test";
}
