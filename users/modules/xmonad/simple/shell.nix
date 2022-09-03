{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = [ pkgs.haskell-language-server pkgs.hlint pkgs.xorg.libXScrnSaver.all ];
}
