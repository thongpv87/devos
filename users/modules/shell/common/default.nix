{ config, lib, pkgs, ... }:
let
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
    ];
    enableWindowsFonts = false;
  };
in
{
  home.packages = with pkgs; [ starship selected-nerdfonts ];

  programs = {
    fzf = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
      fileWidgetOptions = [ "--preview 'head {}'" ];
      #historyWidgetCommand = "fc -l 1 | sort -r --key 2.1 -k1,1nr | uniq -f 1 | sort -rn";
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
      enableZshIntegration = true;
    };
  };

  #home.sessionVariables = {};

  xdg = {
    enable = true;
    configFile."starship.toml".source = ./starship.toml;
  };
}
