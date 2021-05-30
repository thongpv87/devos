{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.module.fonts;
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
  options = {
    module.fonts.enable = mkOption {
      default = false;
    };
  };

  config = mkIf cfg.enable {
    #fonts.fontconfig.enable = true;

    home.packages = with pkgs;
      [
        corefonts
        google-fonts
        roboto-slab

        fira-code
        source-code-pro
        fira-mono
        fira-code-symbols
        inconsolata
        emacs-all-the-icons-fonts
        font-awesome
        selected-nerdfonts
        #powerline-fonts
      ];
  };
}
