{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.module.shell;
in
{
  imports = [
    ./tmux
    ./bash
    ./zsh
  ];

  options = {
    module.shell = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        htop
        gtop
        wtf
        nano
        vim
        xclip
        xsel
        curl
        wget
        ix
        inxi
        powertop
        neofetch
        lolcat
        ncurses
        glxinfo
        lshw
        s-tui
        python3
        silver-searcher
        gawk
        ack
        jq
        yq
        gnused
        nix-prefetch-git
        git-crypt
        gnumake
      ];

      programs.git.enable = true;

      module.shell = {
        zsh.enable = true;
        bash.enable = true;
        tmux = {
          enable = true;
          shell = "zsh";
        };
      };
    }
  ]);
}
