{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.module.media.glava;
in
{
  options = {
    module.media.glava = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        glava
      ];

      xdg.configFile."glava.in" = {
        source = ./glava;
        recursive = true;
      };

      home.activation = {
        glavaActivation = lib.hm.dag.entryAfter ["writeBoundary"] ''
          $DRY_RUN_CMD cp -R $HOME/.config/glava.in $HOME/.config/glava
          chmod -R 660 $HOME/.config/glava
        '';
    };
    }
  ]);
}
