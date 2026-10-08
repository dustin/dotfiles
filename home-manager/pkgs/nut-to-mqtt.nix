{ pkgs }:

pkgs.buildGoModule rec {
  pname = "nut-to-mqtt";
  version = "0.0.3";
  src = pkgs.fetchFromGitHub {
    owner = "jnovack";
    repo = "nut-to-mqtt";
    tag = "v${version}";
    hash = "sha256-D6wDV+uMY4xIgde4uBmfqS7q85UFhKawGWs773uzAGU=";
  };
  # The upstream v1.0.0 tag of github.com/jnovack/go-version was rewritten
  # after nut-to-mqtt pinned it, so it no longer matches go.sum (or
  # sum.golang.org) and can't be fetched. v1.0.1 is the same package.
  patches = [ ./nut-to-mqtt-go-version.patch ];
  vendorHash = "sha256-ndAJjWhnT4DuFJDE+1ngLzm8nYfSuaSZxYK0pbfhUvA=";
  subPackages = [ "cmd/nut-to-mqtt" ];
  ldflags = [
    "-s" "-w"
    "-X github.com/jnovack/go-version.Application=nut-to-mqtt"
    "-X github.com/jnovack/go-version.Version=v${version}"
  ];
  meta.mainProgram = "nut-to-mqtt";
}
