{ suites, lib, config, pkgs, ... }:
let
  cfg = config.personalize.displayServer.xorg;
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
in
with lib; {
  imports = [ ./xmonad.nix ];
  options = {
    personalize.displayServer.xorg = {
      enable = mkEnableOption (mdDoc "Enable Xorg display server");
      gpuMode = mkOption {
        type = with types; enum [ "integrated" "hybrid" "NVIDIA" ];
        default = "hybrid";
      };
    };
  };

  config = mkIf (cfg.enable) (mkMerge [
    {
      nixpkgs.config.allowUnfree = true;
      services.xserver = {
        layout = "us";
        libinput.enable = true;
        xkbModel = "thinkpad";
        xkbOptions = "caps:escape,altwin:prtsc_rwin";
      };
    }

    (mkIf (cfg.gpuMode == "hybrid") {
      environment.systemPackages = [ nvidia-offload ];

      hardware = {
        nvidia = {
          powerManagement = {
            enable = true;
            finegrained = true;
          };
          nvidiaPersistenced = true;
          modesetting.enable = true;
          prime = {
            offload.enable = true;
            intelBusId = "PCI:0:2:0";
            nvidiaBusId = "PCI:1:0:0";
          };
        };
      };
      services.xserver = {
        videoDrivers = [ "nvidia" ];
        deviceSection = ''
          Option "DRI" "3"
          Option "TearFree" "true"
        '';
        enable = true;
      };
    })

    (mkIf (cfg.gpuMode == "NVIDIA") {
      hardware = {
        nvidia = {
          modesetting.enable = true;
          prime = {
            sync.enable = true;
            intelBusId = "PCI:0:2:0";
            nvidiaBusId = "PCI:1:0:0";
          };
        };
      };

      services.xserver = {
        videoDrivers = [ "nvidia" ];
        enable = true;
        screenSection = ''
          Option         "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
          Option         "AllowIndirectGLXProtocol" "off"
          Option         "TripleBuffer" "on"
        '';
      };
    })
  ]);
}
