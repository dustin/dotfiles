{ systemd, config, lib, pkgs, ... }:

{
  launchd.agents.pueue = {
      enable = true;
      config = {
        Label = "net.spy.pueued";
        ProgramArguments = [ "${pkgs.pueue}/bin/pueued" ];
        RunAtLoad = true;
  
        # Relaunch on exit or crash
        KeepAlive = true;
  
        StandardOutPath = "${config.xdg.stateHome}/my-server/stdout.log";
        StandardErrorPath = "${config.xdg.stateHome}/my-server/stderr.log";
  
        EnvironmentVariables = {
          PATH = lib.makeBinPath [ pkgs.pueue ];
          HOME = config.home.homeDirectory;
          LANG = "en_US.UTF-8";
        };
        WorkingDirectory = config.home.homeDirectory;
      };
    };
}
