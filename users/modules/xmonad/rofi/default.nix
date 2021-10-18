{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.module.xmonad.rofi;

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
      "Hurmit-Nerd-Font-Mono"
      "Fantasque-Sans-Mono"
    ];
    enableWindowsFonts = false;
  };
in
{
  options = {
    module.xmonad.rofi = {
      enable = mkOption {
        default = false;
      };

      profile = mkOption {
        type = with types; enum [ "simple" ];
        default = "simple";
        description = ''
          rofi theme"
        '';
      };
    };

  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [ rofi noto-fonts-extra ];

      xdg = {
        configFile = {
          "rofi" = {
            source = ./rofi/1080p;
            recursive = true;
          };
        };

        dataFile = {
          "fonts/Feather.ttf".source = ./rofi/fonts/Feather.ttf;
          "fonts/Hurmit-Nerd-Font-Mono.otf".source = ./rofi/fonts/Hurmit-Nerd-Font-Mono.otf;
        };
      };
    }

    (mkIf (cfg.profile == "simple") { })
  ]);
}
