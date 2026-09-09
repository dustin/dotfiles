{ config, lib, pkgs, ... }:

{
  programs.zsh.initContent = lib.mkBefore ''
    export PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$HOME/local.bin:$HOME/bin:$HOME/.local/bin:$HOME/go/bin:$PATH"
    export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
  '';

  programs.headroom = {
    enable = true;
    proxy = {
      enable = true;
      targetApiUrl = "http://localhost:11434/v1";
      apiKey = "ollama";
      mode = "token";
      disableKompress = true;
      port = 8787;
    };
  };

  home.file = {
    ".local/state/atuin-daemon/.keep".text = "";
  };

  launchd.agents.atuin-daemon = {
    enable = true;
    config = {
      Label = "net.spy.atuin-daemon";
      ProgramArguments = [
        "${config.programs.atuin.package}/bin/atuin"
        "daemon"
        "start"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardOutPath = "${config.xdg.stateHome}/atuin-daemon/stdout.log";
      StandardErrorPath = "${config.xdg.stateHome}/atuin-daemon/stderr.log";
    };
  };

  launchd.agents.pueue = {
      enable = true;
      config = {
        Label = "net.spy.pueued";
        ProgramArguments = [ "${pkgs.pueue}/bin/pueued" ];
        RunAtLoad = true;

        # Relaunch on exit or crash
        KeepAlive = true;

        StandardOutPath = "${config.xdg.stateHome}/pueue/stdout.log";
        StandardErrorPath = "${config.xdg.stateHome}/pueue/stderr.log";

        EnvironmentVariables = {
          PATH = lib.makeBinPath [ pkgs.pueue ];
          HOME = config.home.homeDirectory;
          LANG = "en_US.UTF-8";
        };
        WorkingDirectory = config.home.homeDirectory;
      };
    };

  launchd.agents.nixgc = {
    enable = true;
    config = {
      Label = "net.spy.nixgc";

      ProgramArguments = [
        "${pkgs.nix}/bin/nix-collect-garbage" "--delete-older-than" "30d"
      ];

      RunAtLoad = false;
      StartInterval = 86400;
      KeepAlive = false;

      StandardOutPath  = "${config.xdg.stateHome}/nixgc/stdout.log";
      StandardErrorPath = "${config.xdg.stateHome}/nixgc/stderr.log";
    };
  };
}
