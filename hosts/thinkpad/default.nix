{ suites, pkgs, lib, config, ... }:
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
  imports = [ ./hardware-configuration.nix ] ++ suites.base ++ suites.personal;

  nixpkgs.config.allowUnfree = true;

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    sandboxPaths = [ "/bin/sh=${pkgs.bash}/bin/sh" ];
    maxJobs = lib.mkDefault 12;
  };

  boot = {
    loader = {
      systemd-boot.enable = true;
      systemd-boot.configurationLimit = 10;
      efi.canTouchEfiVariables = true;
    };

    kernel.sysctl = {
      "vm.swappiness" = 1;
    };


    kernelPackages = pkgs.linuxPackages_latest;
    initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "battery" "thinkpad_acpi" ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" "acpi_call" "coretemp" ];
    blacklistedKernelModules = [ ];
    kernelParams = [ "quiet" "intel_pstate=disable" "nvidia.NVreg_DynamicPowerManagement=0x02" ];

    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];

    extraModprobeConfig = lib.mkMerge [
      # enable wifi power saving (keep uapsd off to maintain low latencies)
      "options iwlwifi power_save=1 uapsd_disable=1"
    ];
  };

  environment.systemPackages = with pkgs; [ vulkan-tools nvidia-offload ];

  time.timeZone = "Asia/Ho_Chi_Minh";
  i18n.inputMethod = {
    enabled = "ibus";
    ibus.engines = with pkgs.ibus-engines; [ bamboo ];
  };

  networking = {
    networkmanager.enable = true;
    interfaces.wlp82s0.useDHCP = true;
  };

  hardware = {
    opengl = {
      enable = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        vaapiIntel
        vaapiVdpau
        libvdpau-va-gl
        intel-media-driver
      ];
    };

    nvidia = {
      powerManagement = {
        enable = true;
        finegrained = true;
      };
      modesetting.enable = true;
      #package = config.boot.kernelPackages.nvidiaPackages.beta;
      prime = {
        #sync.enable = true;
        offload.enable = true;
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";
      };
    };
  };

  services = {
    logind.extraConfig = "RuntimeDirectorySize=30%";

    upower.enable = true;
    udev.packages = with pkgs; [ gnome3.gnome-settings-daemon ];
    fstrim.enable = true;

    xserver = {
      videoDrivers = [ "nvidia" ];

      enable = true;
      displayManager.gdm.enable = true;
      windowManager.xmonad.enable = true;
      layout = "us";
      libinput.enable = true;
    };

    #thermald.enable = false;
    undervolt = {
      enable = true;
      coreOffset = -120;
      gpuOffset = -50;
      uncoreOffset = -50;
    };

    thinkfan = {
      enable = true;
      fans = [
        {
          type = "tpacpi";
          query = "/proc/acpi/ibm/fan";
        }
      ];

      sensors = [
        {
          query = "/proc/acpi/ibm/thermal";
          type = "tpacpi";
        }
      ];

      levels = [
        [ 0 0 45 ]
        [ 1 40 47 ]
        [ 2 42 50 ]
        [ 3 44 53 ]
        [ 4 48 58 ]
        [ 5 50 62 ]
        [ 6 52 65 ]
        [ 7 54 80 ]
        [ "level auto" 80 32767 ]
      ];
    };

    tlp = {
      enable = true;
      settings = {
        START_CHARGE_THRESH_BAT0 = 75;
        STOP_CHARGE_THRESH_BAT0 = 80;

        CPU_SCALING_GOVERNOR_ON_AC = "schedutil";
        CPU_SCALING_GOVERNOR_ON_BAT = "schedutil";
        #CPU_SCALING_GOVERNOR_ON_AC="powersave";
        #CPU_SCALING_GOVERNOR_ON_BAT="powersave";

        CPU_SCALING_MIN_FREQ_ON_AC = 800000;
        CPU_SCALING_MAX_FREQ_ON_AC = 3500000;
        CPU_SCALING_MIN_FREQ_ON_BAT = 800000;
        CPU_SCALING_MAX_FREQ_ON_BAT = 2300000;

        #CPU_BOOST_ON_AC=1;
        #CPU_BOOST_ON_BAT=0;

        # Enable audio power saving for Intel HDA, AC97 devices (timeout in secs).
        # A value of 0 disables, >=1 enables power saving (recommended: 1).
        # Default: 0 (AC), 1 (BAT)
        SOUND_POWER_SAVE_ON_AC = 0;
        SOUND_POWER_SAVE_ON_BAT = 1;

        # Runtime Power Management for PCI(e) bus devices: on=disable, auto=enable.
        # Default: on (AC), auto (BAT)
        RUNTIME_PM_ON_AC = "on";
        RUNTIME_PM_ON_BAT = "auto";

        # Battery feature drivers: 0=disable, 1=enable
        # Default: 1 (all)
        NATACPI_ENABLE = 1;
        TPACPI_ENABLE = 1;
        TPSMAPI_ENABLE = 1;

        # Bluetooth devices are excluded from USB autosuspend:
        #   0=do not exclude, 1=exclude
        USB_BLACKLIST_BTUSB = 1;
      };
    };
  };

  systemd.services.thinkfan.preStart = "
    /run/current-system/sw/bin/modprobe  -r thinkpad_acpi && /run/current-system/sw/bin/modprobe thinkpad_acpi
  ";

  bud.enable = true;
  bud.localFlakeClone = "/home/thongpv87/ws/devos";
  system.stateVersion = "21.11";
}
