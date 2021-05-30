{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.module.shell.tmux;
  shellCmd =
    if cfg.shell == "zsh" then "${pkgs.zsh}/bin/zsh"
    else if cfg.shell == "bash" then "${pkgs.bashInteractive}/bin/bash"
    else #if cfg.shell == "fish"
      "${pkgs.fish}/bin/fish";
in
{
  options = {
    module.shell.tmux = {
      enable = mkOption {
        default = false;
        description = ''
          Whether to enable tmux module
        '';
      };

      shell = mkOption {
        type = with types; enum [ "bash" "zsh" "fish" ];
        default = "bash";
      };
    };
  };


  config = mkIf cfg.enable (
    mkMerge [
      {
        programs.tmux = {
          enable = true;
          plugins = with pkgs.tmuxPlugins; [
            resurrect
            sensible
            yank
            prefix-highlight
            #tilish
            pain-control
            # {
            #   plugin = continuum;
            #   extraConfig = ''
            #    # set -g @continuum-restore 'on'
            #    set -g @continuum-save-interval '30' # minutes
            #    set -g @yank_action 'copy-pipe'
            #  '';
            # }

            #themes
            #gruvbox
            {
              plugin = power-theme;
              extraConfig = ''
                set -g @tmux_power_theme 'moon'
                #set -g @tmux_power_theme 'default'
              '';
            }
          ];
          shortcut = "o";
          customPaneNavigationAndResize = true;
          keyMode = "emacs";
          newSession = true;
          secureSocket = false;
          terminal = "xterm-direct";

          extraConfig = ''
            set-option  -g default-shell ${shellCmd}

            ${readFile ./tmux.conf}
            #$#{readFile ./bindings.conf}
          '';
        };
      }
    ]);
}
