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
        overlays = [
          haskellNix.overlay
          (final: prev: {
            hixProject =
              final.haskell-nix.stackProject' {
                name = "xmonad-simple";
                src = ./.;
                evalSystem = "x86_64-linux";
                shell = {
                  buildInputs = with final.xorg;
                    [ libX11 libX11.dev xlibsWrapper libXext
                      libXinerama libXrandr libXrender
                      final.pkgconfig libXft libXrandr
                      libXScrnSaver final.alsa-lib.dev
                    ];
                    exactDeps = true;
                    withHoogle = true;
                    tools = {
                      cabal = "3.2.0.0";
                      hlint = "latest";
                      haskell-language-server = "latest";
                    };
                  };
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake { };
      in
      flake // {
        legacyPackages = pkgs;
      });

  # --- nFlake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
    use-sanbox = false;
  };
}
