{
  config,
  pkgs,
  pkgs-old,
  ...
}:

let
  pycentauri = pkgs.callPackage ../pkgs/pycentauri.nix { };
in
{
  my.secrets = {
    enable = true;
  };

  services.nutToMqtt.enable = true;
  services.loaner.enable = false;
  services.bambuWeightFetcher.enable = true;

  home.packages = with pkgs; [
    static-web-server
    pkgs-old.haskellPackages.net-mqtt # my mqtt-watch command
    pycentauri
  ];
}
