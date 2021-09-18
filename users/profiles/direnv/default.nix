{
  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
      # optional for nix flakes support
      enableFlakes = true;
    };
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
}
