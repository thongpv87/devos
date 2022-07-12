{ suites, lib, config, ... }:
{
  ### root password is empty by default ###
  imports = suites.base;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;


    fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/25D0-D078";
      fsType = "vfat";
    };
    fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/25D0-D078";
      fsType = "vfat";
    };
    
    fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/7e2b80fd-0081-4360-8f33-5395961a8436";
      fsType = "xfs";
    };

    fileSystems."/home" =
    {
      device = "/dev/disk/by-uuid/043d0662-a928-4d4b-8c59-ccf503095f96";
      fsType = "ext4";
    };
}
