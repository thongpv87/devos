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
          #jonaburg-picom
        ];

      services = {
        picom = {
          enable = true;

          vSync = true;

          activeOpacity = "0.95";
          inactiveOpacity = "0.9";
          opacityRule = [
            "100:class_g   *?= 'Chromium-browser'"
            "100:class_g   *?= 'Google-Chrome'"
            "100:class_g   *?= 'zoom'"
            "100:class_g   *?= 'Firefox'"
            "100:class_g   *?= 'gitkraken'"
            "100:name      *?= 'emacs'"
            "100:class_g   *?= 'emacs'"
            "100:class_g   ~=  'jetbrains'"
            #"100:class_g   *?= 'slack'"
          ];

          blur = true;
          blurExclude = [
            "class_g = 'slop'"
          ];
          extraOptions = ''
          corner-radius = 30;
          xinerama-shadow-crop = true;
          #blur-background = true;
          blur-method = "kernel";
          blur-strength = 5;
          '';
          experimentalBackends = true;

          shadowExclude = [
            "bounding_shaped && !rounded_corners"
          ];

          fade = true;
          fadeDelta = 10;
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
          "picom/picom.conf".source = ./config/picom.conf;
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
