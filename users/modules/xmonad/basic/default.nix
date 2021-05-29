{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.xmonad.basic;
in
{
  options = {
    module.xmonad.basic = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs;
        [
          jq
          xclip
          maim
          xdotool
          feh
          scrot
          rofi
          xmobar
          picom
          font-awesome
          selected-nerdfonts
          rofi
          xorg.setxkbmap
          libqalculate
          brightnessctl
          xorg.xbacklight
        ];

      xsession = {
        enable = true;

        profileExtra = ''
          # wal -R&
        '';

        windowManager = {
          xmonad = {
            enable = true;
            enableContribAndExtras = true;
            extraPackages = haskellPackages: with haskellPackages; [ xmonad-wallpaper xmobar ];
            config = ./xmonad.hs;
          };
        };
      };


      programs = {
        rofi = {
          enable = true;
          cycle = true;
          font = "Source Code Pro 12";
          theme = "DarkBlue";
        };
      };

      home.file = {
        ".xmonad/xmobarrc".source = ./xmobarrc;

        ".xmonad/bin" = {
          source = ./bin;
          recursive = true;
        };
      };
    }
  ]);
}
