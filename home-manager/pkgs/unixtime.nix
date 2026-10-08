{ pkgs }:

let
  zig = pkgs.zig_0_15;
in
pkgs.stdenv.mkDerivation (finalAttrs: {
  pname = "unixtime";
  version = "0-unstable-2026-03-02";

  src = pkgs.fetchFromGitHub {
    owner = "dustin";
    repo = "unixtime";
    rev = "59e8378f763cb0ab812925ee26abdb126df797bc";
    hash = "sha256-M2V6lxSn9TVIBPLxtQyjDRX3Jwtgizj+dWH1RU0n3ao=";
  };

  # build.zig.zon dependencies (zeit), fetched up front since the build
  # sandbox has no network. Update `hash` whenever build.zig.zon changes.
  zigDeps = zig.fetchDeps {
    inherit (finalAttrs) src pname version;
    hash = "sha256-J8O6gaUR1rbrxwVdW0wO45l4wPl2ILMH3nNxw+PbmSA=";
  };

  postConfigure = ''
    ln -s ${finalAttrs.zigDeps} "$ZIG_GLOBAL_CACHE_DIR/p"
  '';

  nativeBuildInputs = [ zig ];

  meta.mainProgram = "unixtime";
})
