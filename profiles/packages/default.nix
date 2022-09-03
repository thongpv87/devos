{ config, pkgs, lib, ... }: {
  programs = {
    dconf.enable = true;
    iftop.enable = true;
    iotop.enable = true;
    nano.syntaxHighlight = true;
    zsh.enable = true;
  };

  services = {
    teamviewer.enable = false;
  };

  environment = {
    systemPackages = with pkgs; [
      #utilities packages
      killall
      git
      lm_sensors
      pciutils
      upower
      unzip
      alacritty
      tmux
      bash-completion
      sqlite
      htop
      iotop
      neofetch
      ntfs3g
      zsh
      fish
      xclip
      xsel
      gnused
      gawkInteractive

      emacs
      nano
      vim
      wget
      ascii
      file
      okular
      goldendict
      shared-mime-info
      firefox
      google-chrome
      libreoffice-fresh
      evince
      notify-osd
      libnotify

      fira
      roboto
      fira-code
      fira-mono
      font-awesome
      fira-code-symbols
      inconsolata
      corefonts
      google-fonts
      roboto-slab

      nix-index
      nix-zsh-completions
    ];
    pathsToLink = [ "/share/zsh" ];
  };
}
