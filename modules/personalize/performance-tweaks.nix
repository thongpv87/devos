{ suites, lib, config, pkgs, ... }:
let cfg = config.personalize.performanceTweaks;
in
with lib;
{
  options = {
    personalize.performanceTweaks = {
      enable = mkOption {
        default = true;
      };

      fancontrol = mkOption {
        type = with types; enum [ "auto" "manual" ];
        default = "auto";
      };

      undervolt = mkOption {
        default = false;
      };

      cpuScaling = mkOption {
        type = with types; enum [ "pstate" "schedutil" ];
        description = "CPU performance scaling driver";
        default = "schedutil";
      };
    };
  };

  config = mkIf (cfg.enable) (mkMerge [
    {
      services.upower.enable = true;

      boot.initrd.availableKernelModules = [ "battery" "thinkpad_acpi" ];
      boot.kernelModules = [ "acpi_call" "coretemp" ];
      boot.extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
    }

    (mkIf (cfg.fancontrol == "manual") {
      boot.initrd.availableKernelModules = [ "battery" "thinkpad_acpi" ];
      boot.kernelModules = [ "acpi_call" "coretemp" ];

      services.thinkfan = {
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
          [ 0 0 50 ]
          [ 1 49 52 ]
          [ 2 52 55 ]
          [ 3 53 58 ]
          [ 4 53 63 ]
          [ 5 56 67 ]
          [ 6 57 70 ]
          [ 7 59 85 ]
          [ "level auto" 80 32767 ]
        ];
      };
    })

    (mkIf cfg.undervolt {
      services.undervolt = {
        enable = true;
        coreOffset = -130;
        gpuOffset = -70;
        uncoreOffset = -70;
      };
    })


    {
      boot.kernelModules = [ "coretemp" ];
      boot.kernelParams = if (cfg.cpuScaling == "schedutil") then [ "intel_pstate=disable" ] else [ ];
      boot.extraModprobeConfig = lib.mkMerge [
        # enable wifi power saving (keep uapsd off to maintain low latencies)
        "options iwlwifi power_save=1 uapsd_disable=1"
      ];

      services.tlp = {
        enable = true;
        settings =
          let governor = (if (cfg.cpuScaling == "schedutil") then "schedutil" else "powersave");
          in
          {
            CPU_SCALING_GOVERNOR_ON_AC = governor;
            CPU_SCALING_GOVERNOR_ON_BAT = governor;

            # START_CHARGE_THRESH_BAT0 = 75;
            # STOP_CHARGE_THRESH_BAT0 = 80;

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
    }
  ]);
}
