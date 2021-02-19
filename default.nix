let
  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/ae66c3e40486c0e88a6cefc8d275c248fc6a696c.tar.gz";
    sha256 = "1gw4kdlkmxyil8capnagv41hqmh31hkibidjgy3bxhlljr8xgfkc";
  };

  config = {};

  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
        overrides =
          let
            manualOverrides = haskellPackagesNew: haskellPackagesOld: {
            };

          in
            pkgsNew.lib.fold
              pkgs.lib.composeExtensions
              (old.overrides or (_: _: {}))
              [ (pkgsNew.haskell.lib.packagesFromDirectory {
                    directory = ./nix;
                  }
                )
                manualOverrides
              ];
      }
    );
  };

  pkgs =
    import nixpkgs { inherit config; overlays = [ overlay ]; };

in
  { inherit (pkgs.haskellPackages) pipes-extras;

    shell = (pkgs.haskell.lib.doBenchmark pkgs.haskellPackages.pipes-extras).env;
  }
