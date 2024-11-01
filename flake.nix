{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              haskellPackages.ghcid
              haskellPackages.cabal-install
              haskellPackages.record-dot-preprocessor
              haskellPackages.haskell-language-server
              haskellPackages.fourmolu
              zlib
            ];
          };
        };
      });
}
