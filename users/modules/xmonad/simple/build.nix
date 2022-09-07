{pkgs}:
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = hsPkgsNew: hsPkgsOld: rec {
          xmonad-simple =
            with pkgs; with hsPkgsOld;
              hsPkgsNew.callPackage  (./xmonad.nix {
                #tar = pkgs.libtar;
              });
        };
      };
    };
  };
  nixpkgs = import <pkgs> {inherit config; };
in
{
  xmonad = pkgs.haskellPackages.xmonad-simple;
}
