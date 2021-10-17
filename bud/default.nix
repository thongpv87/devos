{ pkgs, lib, budUtils, ... }: {
  bud.cmds = with pkgs; {
    get = {
      writer = budUtils.writeBashWithPaths [ nixUnstable git coreutils git-crypt ];
      synopsis = "get [DEST]";
      help = "Copy the desired template to DEST";
      script = ./get.bash;
    };
  };
}
