{ pkgs }:

# Written out from `cabal2nix` rather than using `callCabal2nix`, which is
# import-from-derivation and would break evaluating darwin/aarch64 hosts on
# an x86_64-linux CI runner (see check-updates.yml / trackedVersions).
# When bumping `rev`, re-run `cabal2nix` against the new source and update
# the dependency lists below if the .cabal file changed.
let
  drv =
    { mkDerivation, amazonka, amazonka-core, amazonka-s3, base
    , bytestring, conduit, conduit-extra, directory, generic-lens, lens
    , lib, optparse-applicative, process, resourcet, text, time
    , zlib-conduit
    }:
    mkDerivation {
      pname = "papertrails";
      version = "0.1.0.0";
      src = pkgs.fetchFromGitHub {
        owner = "dustin";
        repo = "papertrails";
        rev = "b973c5c9f9eddfd0b6f041424932f2b0c3ccaf67";
        hash = "sha256-aLmi3eOPevpmW70I8r743iDdqjOpjvzC6XeZwQm9HXA=";
      };
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends = [
        amazonka amazonka-core amazonka-s3 base bytestring conduit
        conduit-extra directory generic-lens lens optparse-applicative
        process resourcet text time zlib-conduit
      ];
      homepage = "https://github.com/dustin/papertrails#readme";
      license = lib.licenses.bsd3;
      mainProgram = "papertrails";
    };

  unwrapped = pkgs.haskell.lib.justStaticExecutables (pkgs.haskellPackages.callPackage drv { });
in
# papertrails shells out to `7z` to build its monthly archives.
pkgs.symlinkJoin {
  name = "papertrails-${unwrapped.version}";
  inherit (unwrapped) version;
  paths = [ unwrapped ];
  nativeBuildInputs = [ pkgs.makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/papertrails --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.p7zip ]}
  '';
  meta.mainProgram = "papertrails";
}
