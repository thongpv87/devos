{ suites, lib, config, ... }:
{
  system.stateVersion = "22.05";
  ### root password is empty by default ###
  imports = suites.base;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];


  services = {
    upower.enable = true;
    logind.extraConfig = "RuntimeDirectorySize=30%";
    #udev.packages = with pkgs; [gnome3.gnome-settings-daemon ];

    xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      windowManager.xmonad.enable = true;
      desktopManager.gnome.enable = true;
      layout = "us";
      libinput.enable = true;
    };
  };

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

    fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/97C5-700D";
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
