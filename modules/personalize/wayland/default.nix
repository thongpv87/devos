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
      wl-clipboard
      pipewire
      wireplumber
      slurp
    ];

    programs.xwayland.enable = true;

    hardware = {
      #Completely disable the NVIDIA graphics card and use the integrated graphics processor instead.
      nvidiaOptimus.disable = true;

      # nvidia = {
      #   open = true;
      #   powerManagement = {
      #     enable = true;
      #     finegrained = true;
      #   };
      #   nvidiaPersistenced = true;

      #   modesetting.enable = true;
      #   #package = config.boot.kernelPackages.nvidiaPackages.beta;
      #   prime = {
      #     #sync.enable = true;
      #     offload.enable = true;
      #     intelBusId = "PCI:0:2:0";
      #     nvidiaBusId = "PCI:1:0:0";
      #   };
      # };
    };
  };
}
