{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs.xorg; with pkgs.haskellPackages;
    [ libX11 xlibsWrapper libXext libXinerama
      libXrandr libXrender libXft
      libXScrnSaver pkgs.alsa-lib
      pkgs.haskell-language-server pkgs.hlint
      pkgs.pkgconfig pkgs.xscreensaver
      pkgs.cabal-install
      libXdmcp.dev pkgs.expat.dev libXpm pkgs.wirelesstools
    ] ++

    [ xmonad
      xmonad-contrib
      xmonad-extras
      xmobar
      containers
      directory
      X11
      async
      stm
    ];
}
