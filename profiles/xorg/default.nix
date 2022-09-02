{ suites, lib, config, pkgs, ... }:
let
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
in
{
  environment.systemPackages = with pkgs; [ vulkan-tools ];

  hardware = {
    nvidia = {
      powerManagement = {
        enable = true;
        finegrained = true;
      };
      #modesetting.enable = false;
      nvidiaPersistenced = true;
      #package = config.boot.kernelPackages.nvidiaPackages.beta;
      prime = {
        #sync.enable = true;
        offload.enable = true;
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";
      };
    };
  };
  services.xserver = {
    videoDrivers = [ "nvidia" ];
    enable = true;
    displayManager.gdm.enable = true;
    windowManager.xmonad.enable = true;
    layout = "us";
    libinput.enable = true;
  };
}
