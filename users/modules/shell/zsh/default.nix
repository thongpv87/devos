{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.module.shell.zsh;
  aliases = {
    ssh = "TERM=xterm-256color ssh";
    irssi = "TERM=xterm-256color irssi";
    nano = "emacsclient -t";
    em = "emacsclient -t";
    real-nano = "${pkgs.nano}/out/bin/nano";
  };
in
{
  imports = [ ../common ];

  options = {
    module.shell.zsh = {
      enable = mkOption {
        default = false;
        description = ''
          Whether to enable zsh bundle
        '';
      };
    };
  };

  config = mkIf cfg.enable
    {
      home.packages = with pkgs; [ any-nix-shell ];
      programs.zsh = {
        enable = true;
        enableCompletion = true;
        enableAutosuggestions = true;
        autocd = true;
        defaultKeymap = "emacs";
        dotDir = ".config/zsh";

        history = {
          extended = true;
          share = true;
          size = 5000;
        };

        localVariables = {
          ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=3,bold";
        };

        shellAliases = aliases;

        initExtra = ''
          any-nix-shell zsh --info-right | source /dev/stdin
          export DIRENV_LOG_FORMAT=
          #eval "$(direnv hook zsh)"
          source ~/.config/zsh/zshrc_extra
          #(cat ~/.cache/wal/sequences &)
          eval "$(starship init zsh)"
        '';

        "oh-my-zsh" = {
          enable = true;
          plugins = [ "git" "sudo" "gitignore" "cp" "docker" "colored-man-pages" "safe-paste" ];
        };
      };

      xdg = {
        enable = true;
        configFile."zsh/zshrc_extra".source = ./zshrc_extra;
      };
    };
}
