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
    starship
    deploy-rs
    ;

  selected-nerdfonts = prev.nerdfonts.override {
    fonts = [
      "FiraCode"
      "FiraMono"
      "SourceCodePro"
      "DejaVuSansMono"
      "DroidSansMono"
      "Inconsolata"
      "Iosevka"
      "RobotoMono"
      "Terminus"
    ];
    enableWindowsFonts = false;
  };

  ibus-engines.bamboo = prev.ibus-engines.bamboo.overrideAttrs (oldAttrs: {
    version = "v0.8.0";
    src = final.fetchFromGitHub {
      owner = "BambooEngine";
      repo = "ibus-bamboo";
      rev = "f6406437969b8e40c5e529cad0055d894fad2c7a";
      sha256 = "0bpnflz0ydifzlh9lg2hv96rg2dxag7cw0la2yh42c36mpahiby3";
    };
    buildInputs = oldAttrs.buildInputs ++ [ final.glib final.gtk3 ];
  });

  haskellPackages = prev.haskellPackages.override
    (old: {
      overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (hfinal: hprev:
        let version = prev.lib.replaceChars [ "." ] [ "" ] prev.ghc.version;
        in
        {
          # same for haskell packages, matching ghc versions
          inherit (channels.latest.haskell.packages."ghc${version}")
            haskell-language-server;
        });
    });
}
