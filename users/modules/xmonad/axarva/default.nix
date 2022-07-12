{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.module.xmonad.axarva;
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
  options = {
    module.xmonad.axarva = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs;
        [
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
          dunst
          selected-nerdfonts
          rofi
          xmobar
          jonaburg-picom
          libqalculate
          brightnessctl
          xorg.xbacklight
          xorg.setxkbmap
        ];

      xsession = {
        enable = true;

        profileExtra = ''# wal -R& '';

        windowManager = {
          xmonad = {
            enable = true;
            enableContribAndExtras = true;
            extraPackages = haskellPackages: with haskellPackages; [ xmonad-wallpaper xmobar ];
            config = ./xmonad.hs;
          };
        };
      };

      systemd.user.services.dunst = {
        Unit = {
          Description = "Dunst notification daemon";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          Type = "dbus";
          BusName = "org.freedesktop.Notifications";
          ExecStart = "${pkgs.dunst}/bin/dunst";
        };
      };

      module.xmonad.rofi = {
        enable = true;
        profile = "simple";
      };

      xdg = {
        enable = true;
        configFile = {
          "picom/picom.conf".source = ./config/picom.conf;
          "dunst" = {
            source = ./dunst;
            recursive = true;
          };
          # "rofi" = {
          #   source = ./rofi;
          #   recursive = true;
          # };

          "alacritty/alacritty.yml.in".source = ./alacritty/alacritty.yml;
        };

        dataFile = {
          "fonts/Museo Sans 300.otf".source = ./fonts/Museo-Sans-300.otf;
        };
      };

      home.file = {
        ".xmonad/xmobar" = {
          source = ./xmobar;
          recursive = true;
        };

        ".xmonad/bin" = {
          source = ./bin;
          recursive = true;
        };
      };
    }
  ]);
}
