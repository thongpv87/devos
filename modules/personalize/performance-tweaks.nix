{ suites, lib, config, pkgs, ... }:
let cfg = config.personalize.performanceTweaks;
in with lib; {
  options = {
    personalize.performanceTweaks = {
      enable = mkOption { default = true; };

      fancontrol = mkOption {
        type = with types; enum [ "auto" "manual" ];
        default = "auto";
      };

      undervolt = mkOption { default = false; };

      cpuScaling = mkOption {
        type = with types;
          enum [ "intel_pstate" "intel_cpufreq" "acpi_cpufreq" ];
        description = "CPU performance scaling driver";
        default = "acpi_cpufreq";
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
        fans = [{
          type = "tpacpi";
          query = "/proc/acpi/ibm/fan";
        }];

        sensors = [{
          query = "/proc/acpi/ibm/thermal";
          type = "tpacpi";
        }];

        levels = [
          [ 0 0 44 ]
          [ 1 44 50 ]
          [ 2 47 55 ]
          [ 3 51 60 ]
          [ 4 54 65 ]
          [ 5 58 70 ]
          [ 6 60 75 ]
          [ 7 59 85 ]
          [ "level auto" 80 32767 ]
        ];
      };
    })

    (mkIf cfg.undervolt {
      services.undervolt = {
        enable = true;
        coreOffset = -120;
        gpuOffset = -90;
        uncoreOffset = -70;
      };
    })

    {
      boot.kernelModules = [ "coretemp" ];
      boot.kernelParams = if (cfg.cpuScaling == "intel_pstate") then
        [ ]
      else if (cfg.cpuScaling == "intel_cpufreq") then
        [ "intel_pstate=passive" ]
      else if (cfg.cpuScaling == "acpi_cpufreq") then
        [ "intel_pstate=disable" ]
      else
        [ ];
      boot.extraModprobeConfig = lib.mkMerge [
        # enable wifi power saving (keep uapsd off to maintain low latencies)
        "options iwlwifi power_save=1 uapsd_disable=1"
      ];

      # Gnome 40 introduced a new way of managing power, without tlp.
      # However, these 2 services clash when enabled simultaneously.
      # https://github.com/NixOS/nixos-hardware/issues/260
      services.power-profiles-daemon.enable = false;
      services.tlp = {
        enable = true;
        settings = let
          governor = (if (cfg.cpuScaling != "schedutil") then
            "powersave"
          else
            "schedutil");
          cpuScaling = (if (cfg.cpuScaling == "acpi_cpufreq") then {
            CPU_SCALING_GOVERNOR_ON_AC = "schedutil";
            CPU_SCALING_GOVERNOR_ON_BAT = "schedutil";

            # Don't not use this setting with intel_pstate scaling driver, use CPU_MIN/MAX_PERF_ON_AC/BAT instead
            CPU_SCALING_MIN_FREQ_ON_AC = 800000;
            CPU_SCALING_MAX_FREQ_ON_AC = 3500000;
            CPU_SCALING_MIN_FREQ_ON_BAT = 800000;
            CPU_SCALING_MAX_FREQ_ON_BAT = 2300000;
          } else if (cfg.cpuScaling == "intel_cpufreq") then {
            CPU_SCALING_GOVERNOR_ON_AC = "schedutil";
            CPU_SCALING_GOVERNOR_ON_BAT = "schedutil";

            # Set Intel CPU HWP.EPP /EPB policy
            CPU_ENERGY_PERF_POLICY_ON_AC = "default";
            CPU_ENERGY_PERF_POLICY_ON_BAT = "power";

            # Set min/max P-state for intel CPUs, min/max from min_perf_pct
            CPU_MIN_PERF_ON_AC = 17;
            CPU_MAX_PERF_ON_AC = 89;
            CPU_MIN_PERF_ON_BAT = 17;
            CPU_MAX_PERF_ON_BAT = 55;
            CPU_HWP_DYN_BOOST_ON_AC = 1;
            CPU_HWP_DYN_BOOST_ON_BAT = 0;

          } else if (cfg.cpuScaling == "intel_pstate") then {
            CPU_SCALING_GOVERNOR_ON_AC = "powersave";
            CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
          } else
            { });
        in lib.mkMerge [
          cpuScaling

          {
            # START_CHARGE_THRESH_BAT0 = 75;
            # STOP_CHARGE_THRESH_BAT0 = 80;

            CPU_BOOST_ON_AC = 1;
            CPU_BOOST_ON_BAT = 0;
            # Minimize number of CPU cores under light load conditions
            SCHED_POWER_SAVE_ON_AC = 0;
            SCHED_POWER_SAVE_ON_BAT = 1;

            NMI_WATCHDOG = 0;

            # Enable audio power saving for Intel HDA, AC97 devices (timeout in secs).
            # A value of 0 disables, >=1 enables power saving (recommended: 1).
            # Default: 0 (AC), 1 (BAT)
            SOUND_POWER_SAVE_ON_AC = 0;
            SOUND_POWER_SAVE_ON_BAT = 0;

            # Radio device switching
            DEVICES_TO_DISABLE_ON_BAT_NOT_IN_USE = "bluetooth wwan";

            PLATFORM_PROFILE_ON_BAT = "low-power";
            # Runtime Power Management for PCI(e) bus devices: on=disable, auto=enable.
            # Default: on (AC), auto (BAT)
            RUNTIME_PM_ON_AC = "auto";
            RUNTIME_PM_ON_BAT = "auto";
            RUNTIME_PM_ENABLE = "01:00.0";

            # Battery feature drivers: 0=disable, 1=enable
            # Default: 1 (all)
            NATACPI_ENABLE = 1;
            TPACPI_ENABLE = 1;
            TPSMAPI_ENABLE = 1;

            # Bluetooth devices are excluded from USB autosuspend:
            #   0=do not exclude, 1=exclude
            USB_BLACKLIST_BTUSB = 1;
          }
        ];
      };
    }
  ]);
}
