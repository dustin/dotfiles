{ config, pkgs, lib, ... }:

let
  # Choose the destination path for your config file depending on the platform.
  jjConfig =
    if pkgs.stdenv.hostPlatform.isDarwin then
      # On macOS, put it in "~/Library/Application Support/jj/config.toml"
      "Library/Application Support/jj/config.toml"
    else
      # On Linux, use the XDG standard location, e.g. "~/.config/jj/config.toml"
      ".config/jj/config.toml";
in
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
    xan # csv editor based on xsv
    xz
    delta # diff viewer
  ];

  home = {
    username      = lib.mkDefault "dustin";
    homeDirectory = lib.mkDefault "/home/dustin";
    stateVersion = "24.11";

    file = {
      ".config/bat/config".text = "--style=plain";
	  "${jjConfig}".text = ''
[user]
name = "Dustin Sallings"
email = "dustin@spy.net"

[ui]
default-command = "log"
editor = "vi"
pager = "delta"

[aliases]
here = ["b", "m", "--to", "@-"]
l = ["log", "-r", "::", "--limit", "10"]
push = ["git", "push"]
clone = ["git", "clone", "--colocate"]
fetch = ["git", "fetch"]
'';
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

	# Source the Nix environment.
    if [[ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]; then
        . "$HOME/.nix-profile/etc/profile.d/nix.sh"
    elif [[ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]]; then
        . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
    fi

      '';
      shellAliases = {
        ls = "lsd";
        ll = "lsd -Al --date=relative";
      };
    };
  };
}
