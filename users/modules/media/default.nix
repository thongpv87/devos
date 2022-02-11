{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.module.media;
in
{
  imports = [
    #./cli-visualizer
    ./mopidy
    ./ncmpcpp
  ];

  options = {
    module.media = {
      enable = mkOption {
        default = false;
        description = ''
          Whether to enable xmonad bundle
        '';
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        rhythmbox
        vlc
        shotwell
        pavucontrol
      ];

      module.media.mopidy.enable = true;
      module.media.ncmpcpp.enable = true;
      #module.media.cli-visualizer.enable = true;
    }
  ]);
}
