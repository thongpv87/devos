{ hmUsers, ... }:
{
  home-manager.users = { inherit (hmUsers) thongpv87; };

  users.users.thongpv87 = {
    uid = 1000;
    #password = "nixos";
    hashedPassword = "$6$AONchX2Ea68$V6DBpCuKfE5Hlv0wk945/cDwjTq5FVF7YwlE7BuVUc/EnVkQ6IBLTHVFtDUlKTy0cBlQlSYTYZ3YF.WUv4zxS1";
    description = "Thong Pham";
    
    isNormalUser = true;
    extraGroups = [ "audio" "wheel" "networkmanager" "docker" ];
  };

}