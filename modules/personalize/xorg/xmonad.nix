{ config, lib, pkgs, ... }:
let
  selected-nerdfonts = pkgs.nerdfonts.overrideAttrs (o: {
    version = "2.1.0";
    fonts = [
      "FiraCode"
      "FiraMono"
      "SourceCodePro"
      "DejaVuSansMono"
      "DroidSansMono"
      "Inconsolata"
      "Iosevka"
      "RobotoMono"
      "Terminus"
    ];
    enableWindowsFonts = false;
  });
  shellScripts = pkgs.stdenv.mkDerivation {
    name = "myShellScripts";
    src = ./xmonad-bin;
    phases = "installPhase";
    installPhase = ''
      mkdir -p $out/bin
      cp -r ${./xmonad-bin}/* $out/bin/
      chmod +x $out/bin/*
    '';
  };

in
{
  environment.systemPackages = with pkgs; [
    rofi
    alacritty
    wmctrl
    acpi
    playerctl
    jq
    xclip
    maim
    shellScripts
    font-awesome
    selected-nerdfonts
    brightnessctl
    xorg.xbacklight
    xorg.setxkbmap
    imagemagick

    # feh
    # xdotool
  ];

  services.xserver = {
    displayManager.gdm.enable = false;
    desktopManager.xfce = { enable = true; };
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };
}
