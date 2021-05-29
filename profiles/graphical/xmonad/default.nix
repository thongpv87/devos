{ pkgs, ... }:
let
  selected-nerdfonts = pkgs.nerdfonts.override {
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
  };
in
{
  environment.systemPackages = with pkgs; [
    farbfeld
    xss-lock
    imgurbash2
    maim
    xclip
    xorg.xdpyinfo

    pop-gtk-theme
    numix-icon-theme
    pop-icon-theme
    papirus-icon-theme
    rhythmbox
    vlc
    shotwell
    dconf
    glib.bin
    gnome3.gnome-tweaks
    gnome3.nautilus
    gnome3.evince

    alacritty
    gnome3.gnome-terminal
    wmctrl
    acpi
    playerctl
    jq
    xclip
    maim
    xautolock
    betterlockscreen
    feh
    xdotool
    scrot
    font-awesome
    selected-nerdfonts
    rofi
    xmobar
    libqalculate
    brightnessctl
    xorg.xbacklight
    xlibs.setxkbmap
  ];

  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: with haskellPackages; [ xmonad-wallpaper xmobar ];
    #config = import ./xmonad.hs.nix { inherit pkgs; };
    #config = ./xmonad.hs;
  };

  services.picom = {
    enable = true;
    inactiveOpacity = 0.8;
    settings = {
      "unredir-if-possible" = true;
      "focus-exclude" = "name = 'slock'";
    };
  };

  programs.slock.enable = true;
}
