{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.module.hyprland;
  switch-input-method = pkgs.writeShellScriptBin "switch-input-method" ''
    if [ $(ibus engine) == xkb:us::eng ]; then ibus engine Bamboo; else ibus engine xkb:us::eng ; fi
  '';
in
{
  imports = [ inputs.hyprland.homeManagerModules.default ];
  options = {
    module.hyprland = {
      enable = mkOption {
        default = false;
        description = ''
          Whether to enable hyprland bundle
        '';
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        pop-gtk-theme
        numix-icon-theme
        pop-icon-theme
        papirus-icon-theme
        rhythmbox
        vlc
        shotwell
        dconf
        glib.bin
        goldendict
        gnome3.gnome-tweaks
        gnome3.nautilus
        gnome3.evince
        gnome3.gnome-terminal
        pavucontrol
        wofi
        seatd
        hyprpaper
        pipewire
        wireplumber
        slurp
        wl-clipboard
        switch-input-method
      ];

      programs = {
        waybar = {
          enable = true;
          package = pkgs.waybar.overrideAttrs (oldAttrs: {
            patchPhase = ''
              sed -i '1s/^/\#define HAVE_WLR\n\#define USE_EXPERIMENTAL\n/' include/factory.hpp
              sed -i 's/zext_workspace_handle_v1_activate(workspace_handle_);/const std::string command = "hyprctl dispatch workspace " + name_;\n\tsystem(command.c_str());/g' src/modules/wlr/workspace_manager.cpp
            '';
            mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
          });

          systemd = {
            enable = true;
            target = "hyprland-session.target";
          };
        };
      };

      gtk = {
        enable = true;
      };

      qt = {
        enable = true;
        platformTheme = "gnome";
        style = {
          package = pkgs.pop-gtk-theme;
          name = "pop";
        };
      };

      module.xmonad.rofi = {
        enable = true;
        profile = "simple";
      };

      wayland.windowManager.hyprland = {
        enable = true;
        systemdIntegration = true;
      };

      xdg.configFile = {
        "hypr/hyprland.conf".source = ./hyprland.conf;
        "waybar" = {
          source = ./waybar;
          recursive = true;
        };
        "wofi/style.css".source = ./wofi.css;
        "hypr/hyprpaper.conf".text = ''
          preload=~/.wallpapers/wallpaper.jpg
          wallpaper=eDP-1,~/.wallpapers/wallpaper.jpg
        '';
      };
    }
  ]);
}
