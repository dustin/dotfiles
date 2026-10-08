{ pkgs }:

pkgs.buildGoModule {
  pname = "gitmirror";
  version = "0-unstable-305325580b9f";
  src = pkgs.fetchFromGitHub {
    owner = "dustin";
    repo = "gitmirror";
    rev = "305325580b9f34e3ec08cf24476ae8fe72e8022e";
    hash = "sha256-aqw+KEThwHaaZ4Lb4afp5OAjbEfQeBLQmxppa2ZTXIA=";
  };
  subPackages = [ "." ];
  vendorHash = "sha256-x9SK+CstG9pic9qkkdgrd+OvGax93X1N+oC/PKQ6Abs=";
  meta.mainProgram = "gitmirror";
}
