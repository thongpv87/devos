channels: final: prev: {

  __dontExport = true; # overrides clutter up actual creations

  inherit (channels.latest)
    cachix
    dhall
    discord
    element-desktop
    rage
    nixpkgs-fmt
    qutebrowser
    signal-desktop
    starship;

  # selected-nerdfonts = prev.nerdfonts.override {
  #   fonts = [ "FiraCode" "FiraMono" "SourceCodePro" "DejaVuSansMono" "DroidSansMono"
  #            "Inconsolata" "Iosevka" "RobotoMono" "Terminus" ];
  # };

  haskellPackages = prev.haskellPackages.override {
    overrides = hfinal: hprev:
      let version = prev.lib.replaceChars [ "." ] [ "" ] prev.ghc.version;
      in
      {
        # same for haskell packages, matching ghc versions
        inherit (channels.latest.haskell.packages."ghc${version}")
          haskell-language-server;
      };
  };

}
