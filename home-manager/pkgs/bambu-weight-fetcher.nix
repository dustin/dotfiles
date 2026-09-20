{ pkgs }:

pkgs.writeShellApplication {
  name = "bambu-weight-fetcher";
  runtimeInputs = [ pkgs.python3 ];
  text = ''
    exec python3 ${./bambu-weight-fetcher.py}
  '';
}
