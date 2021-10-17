{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.module.develop.holmusk;
in
{
  options = {
    module.develop.holmusk.enable = mkOption {
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      zoom-us
      dropbox
      tdesktop
      _1password
      _1password-gui
      awscli2
      ssm-session-manager-plugin
      postman
      dbeaver
      nodejs
      elmPackages.create-elm-app
      teams
      terraform_0_14
      ngrok
      packer
      cargo
    ];
  };
}
