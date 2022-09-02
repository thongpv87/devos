{ suites, lib, config, pkgs, ... }:
let
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export DRI_PRIME=pci-0000_01_00_0
    export __VK_LAYER_NV_optimus=NVIDIA_only
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    exec -a "$0" "$@"
  '';
in
{
  environment.systemPackages = with pkgs; [ vulkan-tools nvidia-offload ];



  services.udev.extraRules =
    lib.optionalString (lib.versionOlder config.boot.kernelPackages.kernel.version "5.5") ''
      # Remove NVIDIA USB xHCI Host Controller devices, if present
      ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c0330", ATTR{remove}="1"
      # Remove NVIDIA USB Type-C UCSI devices, if present
      ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c8000", ATTR{remove}="1"
      # Remove NVIDIA Audio devices, if present
      ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x040300", ATTR{remove}="1"
    '' + ''
      # Enable runtime PM for NVIDIA VGA/3D controller devices on driver bind
      ACTION=="bind", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x030000", TEST=="power/control", ATTR{power/control}="auto"
      ACTION=="bind", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x030200", TEST=="power/control", ATTR{power/control}="auto"

      # Disable runtime PM for NVIDIA VGA/3D controller devices on driver unbind
      ACTION=="unbind", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x030000", TEST=="power/control", ATTR{power/control}="on"
      ACTION=="unbind", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x030200", TEST=="power/control", ATTR{power/control}="on"
      '';

  boot.extraModprobeConfig = ''
    options nvidia "NVreg_DynamicPowerManagement=0x02"
  '';
}
