{ hmUsers, ... }:
{
  home-manager.users = { inherit (hmUsers) thongpv87; };

  users.users.thongpv87 = {
    password = "nixos";
    description = "default";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

}