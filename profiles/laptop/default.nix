{ config, pkgs, lib, ... }: {
  environment.systemPackages = with pkgs; [
    acpi
    lm_sensors
    wirelesstools
    pciutils
    usbutils
  ];

  hardware = {
    enableAllFirmware = true;

    pulseaudio = {
      # enable = true;
      package = pkgs.pulseaudioFull;
      extraConfig = "load-module module-switch-on-connect";

      # configFile = pkgs.writeText "default.pa" ''
      #   load-module module-bluetooth-policy
      #   load-module module-bluetooth-discover
      # '';
    };

    bluetooth = {
      enable = true;
      settings = { General = { Enable = "Source,Sink,Media,Socket"; }; };
    };
  };

  services.blueman.enable = true;

  systemd.user.services.mpris-proxy = {
    description = "Mpris proxy";
    after = [ "network.target" "sound.target" ];
    wantedBy = [ "default.target" ];
    serviceConfig = { ExecStart = "${pkgs.bluez}/bin/mpris-proxy"; };
  };

  # to enable brightness keys 'keys' value may need updating per device
  programs.light.enable = true;
  services.actkbd = {
    enable = true;
    bindings = [
      {
        keys = [ 225 ];
        events = [ "key" ];
        command = "/run/current-system/sw/bin/light -A 5";
      }
      {
        keys = [ 224 ];
        events = [ "key" ];
        command = "/run/current-system/sw/bin/light -U 5";
      }
    ];
  };

  # PireWire
  sound.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;
  };
  environment.etc = {
    "wireplumber/bluetooth.lua.d/51-bluez-config.lua".text = ''
      bluez_monitor.properties = {
        ["bluez5.enable-sbc-xq"] = true,
        ["bluez5.enable-msbc"] = true,
        ["bluez5.enable-hw-volume"] = true,
        ["bluez5.headset-roles"] = "[ hsp_hs hsp_ag hfp_hf hfp_ag ]"
      }
    '';
  };

  sound.mediaKeys = lib.mkIf (!config.hardware.pulseaudio.enable) {
    enable = true;
    volumeStep = "1dB";
  };

  # better timesync for unstable internet connections
  services.chrony.enable = true;
  services.timesyncd.enable = false;

  # # power management features
  # services.tlp.enable = true;
  # services.tlp.settings = {
  #   CPU_SCALING_GOVERNOR_ON_AC = "performance";
  #   CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
  #   CPU_HWP_ON_AC = "performance";
  # };

  services.logind.lidSwitch = "suspend";

  nixpkgs.overlays = let
    light_ov = self: super: {
      light = super.light.overrideAttrs (o: {
        src = self.fetchFromGitHub {
          owner = "haikarainen";
          repo = "light";
          rev = "ae7a6ebb45a712e5293c7961eed8cceaa4ebf0b6";
          sha256 = "00z9bxrkjpfmfhz9fbf6mjbfqvixx6857mvgmiv01fvvs0lr371n";
        };
      });
    };
  in [ light_ov ];
}
