{ pkgs }:

# Run a command over and over until it exits successfully.
pkgs.writeShellApplication {
  name = "untilsuccessful";
  runtimeInputs = [ pkgs.coreutils ];
  text = ''
    while ! "$@"
    do
        sleep 1
        echo Retrying...
    done
  '';
}
