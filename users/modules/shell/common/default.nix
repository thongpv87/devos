{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [ starship selected-nerdfonts ];

  programs = {
    fzf = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
      fileWidgetOptions = [ "--preview 'head {}'" ];
    };

    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
        enableFlakes = true;
      };
      enableZshIntegration = true;
    };
  };

  #home.sessionVariables = {};

  xdg = {
    enable = true;
    configFile."starship.toml".source = ./starship.toml;
  };
}
