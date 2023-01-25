{ config, lib, pkgs, ... }:
with lib;
let cfg = config.module.develop.agda;
in {
  options = { module.develop.agda.enable = mkOption { default = false; }; };

  config =
    mkIf cfg.enable { home.packages = with pkgs.agdaPackages; [ agda ]; };
}
