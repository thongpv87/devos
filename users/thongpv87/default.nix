{ hmUsers, pkgs, ... }: {
  home-manager.users = { inherit (hmUsers) thongpv87; };

  users.users.thongpv87 = {
    uid = 1000;
    #password = "nixos";
    #hashedPassword = "$6$AONchX2Ea68$V6DBpCuKfE5Hlv0wk945/cDwjTq5FVF7YwlE7BuVUc/EnVkQ6IBLTHVFtDUlKTy0cBlQlSYTYZ3YF.WUv4zxS1";
    hashedPassword =
      "$6$oyRKMGkCPhmlWtbr$TNVo8.jCNf3pQAHgE3b3S6kVGaB4Fg4T3YqDqwJcANGg3YwNptyTfrmI/qJ5bLtr4PofPC1a3nxMFb.4ik07t0";
    description = "Thong Pham";
    shell = pkgs.zsh;

    isNormalUser = true;
    extraGroups = [ "audio" "wheel" "networkmanager" "docker" ];
  };

  fileSystems."/home/thongpv87/Dropbox/Documents" = {
    device = "/home/thongpv87/Documents";
    options = [ "bind" ];
  };
}
