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
    src = ./bin;
    phases = "installPhase";
    installPhase = ''
      mkdir -p $out/bin
      cp -r ${./bin}/* $out/bin/
      chmod +x $out/bin/*
    '';
  };

in
{
  environment.systemPackages = with pkgs; [
    alacritty
    wmctrl
    acpi
    playerctl
    jq
    xclip
    font-awesome
    brightnessctl
    imagemagick
    selected-nerdfonts
    plasma5Packages.bismuth
    wayland-utils
    elisa
    gwenview
    okular
    konversation

    # xmonad pkgs
    jq
    xclip
    feh
    rofi
    brightnessctl
    xorg.xbacklight
    xorg.setxkbmap
    font-awesome
    imagemagick
    shellScripts
  ];

  programs.xwayland.enable = true;

  services.xserver = {
    displayManager.gdm.enable = true;
    desktopManager.plasma5 = {
      enable = true;
      useQtScaling = true;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };
}
