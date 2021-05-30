{ config, pkgs, lib, ... }: {
  programs = {
    dconf.enable = true;
    iftop.enable = true;
    iotop.enable = true;
    nano.syntaxHighlight = true;
    zsh.enable = true;
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
      shared_mime_info
      firefox
      google-chrome
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
