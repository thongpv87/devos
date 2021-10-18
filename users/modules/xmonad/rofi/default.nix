{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.module.xmonad.rofi;
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
          "fonts" = {
            source = ./rofi/fonts;
            recursive = true;
          };
        };
      };
    }

    (mkIf (cfg.profile == "simple") { })
  ]);
}
