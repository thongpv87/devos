{ pkgs, ... }: {
  #environment.systemPackages = [ pkgs.pinentry ];
  services.fprintd.enable = true;

  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  #   enableBrowserSocket = true;
  # };
}
