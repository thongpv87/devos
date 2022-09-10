{ suites, lib, config, pkgs, ... }:
{
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
        gpuMode = "hybrid";
      };
      wayland = {
        enable = false;
      };
    };
  };

  nixpkgs.config.allowUnfree = true;

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
    kernelParams = [ "quiet" "msr.allow_writes=on" "cpuidle.governor=teo" ];

    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
  };


  time.timeZone = "Asia/Ho_Chi_Minh";
  i18n.inputMethod = {
    enabled = "ibus";
    ibus.engines = with pkgs.ibus-engines; [ bamboo ];
  };

  networking.networkmanager.enable = true;

  hardware = {
    opengl = {
      enable = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        vaapiIntel
        vaapiVdpau
        libvdpau-va-gl
        intel-media-driver
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

    fwupd.enable = true;
    udev.packages = with pkgs; [ gnome3.gnome-settings-daemon ];
  };

  # systemd.services.thinkfan.preStart = "
  #   /run/current-system/sw/bin/modprobe  -r thinkpad_acpi && /run/current-system/sw/bin/modprobe thinkpad_acpi
  # ";


  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/97C5-700D";
      fsType = "vfat";
    };

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/e50d10de-636c-4f47-bf0e-66d8a240efe3";
      fsType = "ext4";
    };

  fileSystems."/home" =
    {
      device = "/dev/disk/by-uuid/5d53d8b7-a579-474b-bba8-56bce1b599c2";
      fsType = "ext4";
    };
}
