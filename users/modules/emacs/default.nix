{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.module.emacs;
in
{
  options = {
    module.emacs = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ sqlite ispell multimarkdown libgccjit ];

    programs.emacs = {
      enable = true;
      #package = pkgs.emacsGcc;
    };

    # home.file.".emacs.d/private/themes" = {
    #   source = ./themes;
    #   recursive = true;
    # };


    services = {
      emacs = {
        enable = true;
        socketActivation.enable = true;
        client.enable = true;
      };
    };
  };
}
