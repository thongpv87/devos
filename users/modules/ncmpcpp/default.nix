{ config, pkgs, lib, ... }:
let
  music-hub = pkgs.writeShellScriptBin "music-hub" ''
    systemctl --user start mopidy
    exec alacritty --class music-hub --title ncmpcpp -e ncmpcpp $@
  '';
in
{
  home.packages = with pkgs; [ ncmpcpp music-hub ];

  xdg.configFile."ncmpcpp" = {
    source = ./config/mechanical_love;
    recursive = true;
  };
}
