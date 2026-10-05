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
  ];

  enterTest = "just test";
}
