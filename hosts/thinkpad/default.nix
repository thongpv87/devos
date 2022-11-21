{ suites, lib, config, pkgs, ... }:
let
  my-bamboo = pkgs.ibus-engines.bamboo.overrideAttrs (oldAttrs: {
    version = "v0.8.1";
    src = pkgs.fetchFromGitHub {
      owner = "BambooEngine";
      repo = "ibus-bamboo";
      rev = "c0001c571d861298beb99463ef63816b17203791";
      sha256 = "sha256-7qU3ieoRPfv50qM703hEw+LTSrhrzwyzCvP9TOLTiDs=";
    };
    buildInputs = oldAttrs.buildInputs ++ [ pkgs.glib pkgs.gtk3 ];
  });
  hybridVaApiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
in {
  system.stateVersion = "22.05";
  ### root password is empty by default ###
  imports = suites.base;
  personalize = {
    performanceTweaks = {
      cpuScaling = "intel_cpufreq";
      undervolt = true;
      fancontrol = "manual";
    };

    displayServer = {
      xorg = {
        enable = true;
        # gpuMode = "NVIDIA";
        gpuMode = "hybrid";
      };
      wayland = { enable = false; };
    };
  };

  nixpkgs.config.allowUnfree = true;

  boot = {
    loader = {
      systemd-boot.enable = true;
      systemd-boot.configurationLimit = 10;
      efi.canTouchEfiVariables = true;
    };

    kernel.sysctl = { "vm.swappiness" = 1; };

    kernelPackages = pkgs.linuxPackages_latest;
    initrd.availableKernelModules =
      [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "battery" "thinkpad_acpi" ];
    initrd.kernelModules = [ "i915" ];
    kernelModules = [ "kvm-intel" "acpi_call" "coretemp" ];
    blacklistedKernelModules = [ ];
    kernelParams = [ "quiet" "msr.allow_writes=on" "cpuidle.governor=teo" ];

    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
  };

  time.timeZone = "Asia/Ho_Chi_Minh";
  i18n.inputMethod = {
    enabled = "ibus";
    ibus.engines = with pkgs.ibus-engines; [ my-bamboo ];
    #enabled = "fcitx";

    fcitx.engines = [ pkgs.fcitx-engines.unikey ];
    fcitx5.addons = [ pkgs.fcitx5-unikey ];
  };

  networking.networkmanager.enable = true;

  hardware = {
    trackpoint.device = "TPPS/2 Elan TrackPoint";
    opengl = {
      enable = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        hybridVaApiIntel
        vaapiVdpau
        libvdpau-va-gl
        intel-media-driver
        nvidia-vaapi-driver
        libva
      ];
    };
  };

  services = {
    fstrim.enable = true;
    logind = {
      extraConfig = ''
        RuntimeDirectorySize=30%
        HandlePowerKey=suspend
        IdleAction=suspend
        IdleActionSec=300
      '';
      lidSwitch = "suspend";
    };

    xserver = {
      layout = "us";
      libinput.enable = true;
      xkbModel = "thinkpad";
      xkbOptions = "caps:escape,altwin:prtsc_rwin";
    };

    fwupd.enable = true;
    udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];
  };

  console.useXkbConfig = true;

  environment.variables = {
    VDPAU_DRIVER =
      lib.mkIf config.hardware.opengl.enable (lib.mkDefault "va_gl");
    LIBVA_DRIVER_NAME = "nvidia";
    MOZ_DISABLE_RDD_SANDBOX = "1";
  };

  # systemd.services.thinkfan.preStart = "
  #   /run/current-system/sw/bin/modprobe  -r thinkpad_acpi && /run/current-system/sw/bin/modprobe thinkpad_acpi
  # ";

  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/97C5-700D";
    fsType = "vfat";
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/e50d10de-636c-4f47-bf0e-66d8a240efe3";
    fsType = "ext4";
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/5d53d8b7-a579-474b-bba8-56bce1b599c2";
    fsType = "ext4";
  };
}
