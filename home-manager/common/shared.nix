{ config, pkgs, lib, ... }:

{
  # Common packages
  home.packages = with pkgs; [
    sqlite-interactive
    mosh
    ffmpeg
    mtr
    p7zip
    rclone
    jq
    asciinema
    exiftool
    gnupg
    watch
    libiconv
    cacert
    magic-wormhole-rs # moving stuff
    age # encryption
    minisign # signing stuff
    jujutsu # jj git thing
    btop # bee top
    kitty # terminal graphics?  why not
    bat # show files.  Not really much to do with concatenation
    lsd # ls
    dust # disk usage some thing
    duf # df thing
    fzf # fuzzy finder
    fd # find
    ripgrep # rg
    bottom # btm top thing
    gping # graphical ping
    procs # ps
    xsv # csv thingy
    xz
  ];

  home = {
    username      = lib.mkDefault "dustin";
    homeDirectory = lib.mkDefault "/home/dustin";
    stateVersion = "24.11";

    file = {
      ".config/bat/config".text = "--style=plain";
    };
  };

  programs = {
    home-manager.enable = true;

    git.enable = true;

    direnv = {
      enable = true;
      enableZshIntegration = true;
    };

    tmux = {
      enable = true;
      clock24 = true;
      historyLimit = 50000;
      extraConfig = ''
        set -g status-right '#(echo $USER) @ #h %a %Y-%m-%d %H:%M'
        setw -g allow-rename on
      '';
    };

    zsh = {
      enable = true;
      initExtra = ''
         autoload -Uz select-word-style
         select-word-style bash
         setopt no_share_history
         unsetopt share_history
      '';
      shellAliases = {
        ls = "lsd";
        ll = "lsd -Al --date=relative";
      };
    };
  };
}
