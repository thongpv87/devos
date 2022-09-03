{ self, config, lib, pkgs, suites, ... }:
with lib;
let cfg = config.personalize;
in
{
  imports = [
    ./xorg
    ./wayland
    ./performance-tweaks.nix
  ];

  options = {
    personalize = { };
  };

  config = mkMerge [
    {
      environment.systemPackages = with pkgs; [ vulkan-tools ];
    }

    {
      # display server
      assertions =
        [{
          assertion = !(cfg.displayServer.xorg.enable && cfg.displayServer.wayland.enable);
          message = "Xorg and Wayland can not be both enabled";
        }];
    }
  ];
}
