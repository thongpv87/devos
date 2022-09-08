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
        default = "zsh";
      };
    };
  };


  config = mkIf cfg.enable (
    mkMerge [
      {
        home.packages = [ pkgs.xsel ];
        programs.tmux = {
          enable = true;
          plugins = with pkgs.tmuxPlugins; [
            #resurrect
            sensible
            yank
            prefix-highlight
            pain-control
            {
              plugin = gruvbox;
              extraConfig = ''
                set -g @tmux_gruvbox 'light'
              '';
            }
          ];
          baseIndex = 1;
          shortcut = "o";
          customPaneNavigationAndResize = true;
          keyMode = "emacs";
          newSession = true;
          secureSocket = false;
          terminal = "xterm-256color";

          extraConfig = ''
            set-option  -g default-shell ${shellCmd}
            set -s copy-command 'xsel -ib'
            ${readFile ./bindings.conf}
            ${readFile ./tmux.conf}
          '';
        };
      }
    ]);
}
