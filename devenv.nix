{ pkgs, ... }:
let
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
  packages = [
    pkgs.just
    roc
  ];

  enterTest = "just test";
}
