{ compiler ? "ghc7103" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              quickdata =
                pkgs.haskell.lib.addBuildDepend
                (haskellPackagesNew.callPackage ./default.nix { })
                haskellPackagesNew.semigroups;

              datetime =
                haskellPackagesNew.callPackage ./datetime.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { quickdata = pkgs.haskell.packages.${compiler}.quickdata;
  }
