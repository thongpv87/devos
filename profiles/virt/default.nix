{ pkgs, ... }: {
  virtualisation = {
    libvirtd = {
      enable = true;
      qemu.runAsRoot = false;
      allowedBridges = [
        "virbr0"
        "virbr1"
      ];
    };
    docker.enable = true;

    containers.enable = true;

    podman.enable = true;
    oci-containers.backend = "podman";

    virtualbox = {
      host.enable = true;
    };
  };

  users.extraGroups.vboxusers.members = [ "thongpv87" ];


  environment = {
    # you'll need to add your user to 'libvirtd' group to use virt-manager
    systemPackages = with pkgs; [ virt-manager vagrant ];

    shellAliases.docker = "podman";

    sessionVariables = {
      VAGRANT_DEFAULT_PROVIDER = "libvirt";
    };
  };
}
