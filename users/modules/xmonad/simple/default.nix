{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.module.xmonad.simple;
in
{
  options = {
    module.xmonad.simple = {
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
          selected-nerdfonts
          rofi
          xmobar
          libqalculate
          brightnessctl
          xorg.xbacklight
          xlibs.setxkbmap
          dunst
        ];

      services = {
        picom = {
          enable = false;
          vSync = true;
        };
      };

      xsession = {
        enable = true;

        profileExtra = ''#wal -R& '';

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

      xdg = {
        configFile = {
          "dunst" = {
            source = ./dunst;
            recursive = true;
          };

          "alacritty/alacritty.yml.in".source = ./alacritty/alacritty.yml;
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
