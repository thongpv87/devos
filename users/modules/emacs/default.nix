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
      package = pkgs.emacsGcc;
      extraPackages = (epkgs: with epkgs; [ vterm ]);
      # package = with pkgs; emacsGit.overrideAttrs (
      #   old: {
      #     configureFlags = old.configureFlags ++ lib.singleton "--with-native-compilation";
      #     buildInputs = with pkgs; old.buildInputs ++ [ libgccjit gcc ];
      #   }
      # );
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
