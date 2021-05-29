{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.module.develop.haskell;
in
{
  options = {
    module.dev.haskell.enable = mkOption {
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home.file = {
      ".cabal/config".source = ./toolchain/cabal.config;
      ".stack/config.yaml".source = ./toolchain/stack.config.yaml;
    };
  };
}
