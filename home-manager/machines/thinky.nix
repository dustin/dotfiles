{ systemd, config, pkgs, ... }:

{
  imports = [
    ../common/shared.nix
    ../common/linux.nix
  ];

  home.packages = with pkgs; [
    symbola
    unifont
    dejavu_fonts
    # python
    # docker-compose

    # k9s

    ncdu
    dmenu
    xmobar
    brightnessctl
    vscode
  ];

  services = {
    emacs.enable = true;
    redshift = {
      enable = true;
      settings.redshift.brightness-night = "0.5";
      latitude = "20.78";
      longitude = "-156.46";
    };
  };

  programs = {
    emacs.enable = true;

    firefox.enable = true;

    git.enable = true;

    urxvt = {
      enable = true;
      keybindings = {
        "Shift-Control-C" = "eval:selection_to_clipboard";
        "Shift-Control-V" = "eval:paste_clipboard";
      };
      scroll.bar.enable = false;
      iso14755 = false; # ☠ 🎔
      fonts = [
        "xft:inconsolata:pixelsize=16:antialias=true:hinting=true"
        "xft:Inconsolata for Powerline:pixelsize=16"
        "xft:Quivira:pixelsize=16"
      ];
      extraConfig = {
        foreground = "white";
        background = "black";
      };
    };
    vscode = {
      enable = true;
      extensions = with pkgs.vscode-extensions; [
        # dracula-theme.theme-dracula
        # vscodevim.vim
        # yzhang.markdown-all-in-one
      ];
    };
  };
}
