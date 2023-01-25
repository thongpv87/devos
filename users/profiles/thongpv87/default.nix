{ pkgs, ... }: {
  manual.manpages.enable = true;

  home.packages = [ pkgs.neovim ];
  home.stateVersion = "22.11";
  programs = {
    git = {
      userName = "Thong Pham";
      userEmail = "thongpv87@gmail.com";
      signing.key = "087E304C48EC0C8D";
    };
  };

  module = {
    hyprland.enable = false;
    xmonad = {
      enable = false;
      rofi.enable = true;
      theme = "simple";
    };

    develop.haskell.enable = true;
    develop.agda.enable = true;
    fonts.enable = true;
    shell.enable = true;
    others.enable = true;
    emacs.enable = true;
    gsettings.enable = true;
    mime.enable = false;
    media.glava.enable = true;
  };

  home = {
    keyboard = {
      model = "thinkpad";
      layout = "us";
      options = [ "caps:escape" "altwin:prtsc_rwin" ];
    };

    language = { base = "en_US.UTF8"; };

    #enableNixpkgsReleaseCheck = true;
    # Note, these variables may be set in any order
    # so no session variable may have a runtime dependency
    # on another session variable
    sessionVariables = { EDITOR = "nvim"; };
  };
}
