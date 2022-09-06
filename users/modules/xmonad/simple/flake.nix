{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "x86_64-linux"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            final.haskellPackages.X11 = prev.haskellPackages.X11.overrideAttrs (o: {
              buildInputs = o.buildInputs ++ [ final.xorg.libX11 final.xlibsWrapper final.xorg.libX11.dev final.xlibsWrapper.dev final.gcc ];
            });

            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = "x86_64-linux";
                shell = {
                  buildInputs = with final.xorg;
                    [ libX11 xlibsWrapper libXScrnSaver libXext libXinerama
                      libXrender libXft libXrandr final.xscreensaver
                      final.pkgconfig final.alsa-lib xrandr final.dbus final.gcc
                    ];
                };
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // {
        legacyPackages = pkgs;
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
    useSanbox = false;
  };
}
