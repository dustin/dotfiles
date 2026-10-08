{ pkgs }:

# Written out from `cabal2nix` rather than using `callCabal2nix`, which is
# import-from-derivation and would break evaluating darwin/aarch64 hosts on
# an x86_64-linux CI runner (see check-updates.yml / trackedVersions).
# When bumping `rev`, re-run `cabal2nix` against the new source and update
# the dependency lists below if the .cabal file changed.
let
  drv =
    { mkDerivation, async, attoparsec, base, HTTP, http-conduit, lib
    , mtl, network, optparse-applicative, QuickCheck, safe-exceptions
    , tasty, tasty-quickcheck, text, time, unbounded-delays
    }:
    mkDerivation {
      pname = "waitforsocket";
      version = "0.1.0.0";
      src = pkgs.fetchFromGitHub {
        owner = "dustin";
        repo = "waitforsocket";
        rev = "5c36a6b565c249e51cc36e1a234a75b765c4c9f9";
        hash = "sha256-eqgCCopOtsSFGkx3u/nJJLchWd8bbzRdGDuVVaOUfQE=";
      };
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [
        async attoparsec base HTTP network optparse-applicative text time
      ];
      executableHaskellDepends = [
        async base http-conduit mtl network optparse-applicative
        safe-exceptions unbounded-delays
      ];
      testHaskellDepends = [
        attoparsec base network QuickCheck tasty tasty-quickcheck text
      ];
      homepage = "https://github.com/dustin/waitforsocket#readme";
      license = lib.licenses.bsd3;
      mainProgram = "waitforsocket";
    };
in
pkgs.haskell.lib.justStaticExecutables (pkgs.haskellPackages.callPackage drv { })
