{ suites, lib, config, pkgs, ... }:
let
  cfg = config.personalize.displayServer.wayland;
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export DRI_PRIME=pci-0000_01_00_0
    export __VK_LAYER_NV_optimus=NVIDIA_only
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    exec -a "$0" "$@"
  '';
in
with lib;
{
  options = {
    personalize.displayServer.wayland.enable = mkEnableOption (mdDoc "Enable Wayland display server");
  };

  config = mkIf (cfg.enable) {
    environment.systemPackages = with pkgs; [
      vulkan-tools
      nvidia-offload
      wlr-randr
      #wl-clipboard
      pipewire
      wireplumber
      slurp
    ];

    programs.xwayland.enable = true;

    hardware = {
      nvidiaOptimus.disable = true;
    };
  };
}
