{
  config,
  pkgs,
  pkgs-old,
  ...
}:

{
  my.secrets = {
    enable = true;
  };

  services.nutToMqtt.enable = true;
  services.loaner.enable = true;

  home.packages = with pkgs; [
    static-web-server
    pkgs-old.haskellPackages.net-mqtt # my mqtt-watch command
  ];
}
