{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.module.xmonad.simple;

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

  xmonadBin = "${pkgs.runCommandLocal "update-xmonad" {}
    ''
      wm="${with pkgs; with haskellPackages; pkgs.haskellPackages.callPackage ./xmonad.nix {}}"
      mkdir -p $out/bin
      cp $wm/bin/xmonadwm $out/bin/xmonad-${pkgs.stdenv.hostPlatform.system};
    ''
  }/bin/xmonad-${pkgs.stdenv.hostPlatform.system}";
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
          xorg.setxkbmap
          dunst
          font-awesome
          selected-nerdfonts
          gnome3.gnome-terminal
          shellScripts
          #xmonadBin
          #jonaburg-picom
        ];

      module.media.enable = false;

      services = {
        picom = {
          enable = true;

          vSync = true;

          #activeOpacity = "1";
          #inactiveOpacity = "0.9";
          opacityRule = [
            "100:class_g   *?= 'Chromium-browser'"
            "100:class_g   *?= 'Google-Chrome'"
            "100:class_g   *?= 'zoom'"
            "100:class_g   *?= 'Firefox'"
            "100:class_g   *?= 'Alacritty'"
            "100:name      *?= 'Dunst'"
            "100:class_g   *?= 'gitkraken'"
            "100:name      *?= 'emacs'"
            "100:class_g   *?= 'emacs'"
            "100:class_g   ~=  'jetbrains'"
            "100:class_g   *?= 'rofi'"
            "70:name       *?= 'GLava'"
            "70:name       *?= 'GLavaRadial'"
          ];

          blur = false;
          blurExclude = [
            "class_g = 'slop'"
          ];
          extraOptions = ''
            corner-radius = 12;
            xinerama-shadow-crop = true;
            #blur-background = true;
            #blur-method = "kernel";
            #blur-strength = 5;
            rounded-corners-exclude = [
              #"window_type = 'normal'",
              "class_g = 'Rofi'",
              #"class_g = 'Tint2'",
              "name = 'Notification area'",
              "name = 'xmobar'",
              "class_g = 'xmobar'",
              #"class_g = 'kitty'",
              #"class_g = 'Alacritty'",
              "class_g = 'Polybar'",
              "class_g = 'code-oss'",
              "class_g = 'firefox'",
              "class_g = 'Thunderbird'"
            ];
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
          command = xmonadBin;
          # command =
          #   let xmonad =  with pkgs; with haskellPackages; pkgs.haskellPackages.callPackage ./xmonad.nix{};
          #   in "${xmonad}/bin/xmonad-x86_64-linux";

          # xmonad = {
          #   enable = true;
          #   package = with pkgs; with haskellPackages; pkgs.haskellPackages.callPackage ./xmonad.nix{};
          #   # enableContribAndExtras = true;
          #   # extraPackages = hsPkgs: [ hsPkgs.xmobar ];
          #   #config = ./xmonad.hs.old;
          # };
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
          #"picom/picom.conf".source = ./config/picom.conf;
          "dunst" = {
            source = ./dunst;
            recursive = true;
          };

          "xmonad" = {
            source = ./.;
            recursive = true;
          };

          "alacritty/alacritty.yml.in".source = ./alacritty/alacritty.yml;
        };
      };

      home.file = {
        ".xmonad/xmonad-${pkgs.stdenv.hostPlatform.system}" = {
          source = xmonadBin;
          onChange = ''
            # Attempt to restart xmonad if X is running.
            if [[ -v DISPLAY ]]; then
              ${config.xsession.windowManager.command} --restart
              fi
          '';
        };
        # ".xmonad/xmobar" = {
        #   source = ./xmobar;
        #   recursive = true;
        # };

        # ".xmonad/bin" = {
        #   source = ./bin;
        #   recursive = true;
        # };
      };
    }
  ]);
}
