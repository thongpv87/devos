{
  manual.manpages.enable = true;

  programs = {
    git = {
      userName = "Thong Pham";
      userEmail = "thongpv87@gmail.com";
    };
  };

  module = {
    xmonad = {
      enable = true;
      theme = "simple";
      #theme = "axarva";

      rofi = {
        enable = true;
        profile = "simple";
      };
    };

    develop.haskell.enable = true;
    #develop.holmusk.enable = true;
    fonts.enable = true;
    shell.enable = true;
    others.enable = true;
    emacs.enable = true;
    gsettings.enable = true;
    #mime.enable = false;
    media.glava.enable = true;
  };

  home = {
    keyboard = {
      model = "thinkpad";
      layout = "us";
      options = [ "ctrl:nocaps" "altwin:prtsc_rwin" ];
    };

    language = {
      base = "en_US.UTF8";
    };

    #enableNixpkgsReleaseCheck = true;
    # Note, these variables may be set in any order
    # so no session variable may have a runtime dependency
    # on another session variable
    sessionVariables = { };
  };
}
