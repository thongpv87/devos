channels: final: prev: {

  __dontExport = true; # overrides clutter up actual creations

  inherit (channels.latest)
    cachix
    dhall
    discord
    element-desktop
    rage
    nix-index
    nixpkgs-fmt
    qutebrowser
    signal-desktop
    starship
    deploy-rs
    zoom-us
    ;

  selected-nerdfonts = prev.nerdfonts.overrideAttrs (o: {
    version = "2.1.0";
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
  });

  ibus-engines.my-bamboo = prev.ibus-engines.bamboo.overrideAttrs (oldAttrs: {
    version = "v0.8.1";
    src = final.fetchFromGitHub {
      owner = "BambooEngine";
      repo = "ibus-bamboo";
      rev = "c0001c571d861298beb99463ef63816b17203791";
      sha256 = "0bpnflz0ydifzlh9lg2hv96rg2dxag7cw0la2yh42c36mpahiby3";
    };
    buildInputs = oldAttrs.buildInputs ++ [ final.glib final.gtk3 ];
  });

  jonaburg-picom = prev.picom.overrideAttrs (oldAttrs: {
    version = "next";
    src = final.fetchFromGitHub {
      owner = "jonaburg";
      repo = "picom";
      rev = "a8445684fe18946604848efb73ace9457b29bf80";
      sha256 = "154s67p3lxdv9is3lnc32j48p7v9n18ga1j8ln1dxcnb38c19rj7";
      fetchSubmodules = true;
    };
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
