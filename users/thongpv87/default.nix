{ ... }:
{
  home-manager.users.thongpv87 = { suites, ... }: {
    imports = suites.base;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.thongpv87 = {
    uid = 1000;
    password = "1234";
    isNormalUser = true;
    extraGroups = [ "audio" "wheel" "networkmanager" ];
  };

  nix.trustedUsers = [ "root" "thongpv87" ];
}
