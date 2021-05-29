{ config, pkgs, lib, ... }:
with pkgs;
with lib;
let
  mopidyEnv = buildEnv {
    name = "mopidy-with-extensions-${mopidy.version}";
    paths = closePropagation [ mopidy-youtube mopidy-mpris mopidy-mpd mopidy-local mopidy-spotify ];
    pathsToLink = [ "/${mopidyPackages.python.sitePackages}" ];
    buildInputs = [ makeWrapper ];
    postBuild = ''
      makeWrapper ${mopidy}/bin/mopidy $out/bin/mopidy \
        --prefix PYTHONPATH : $out/${mopidyPackages.python.sitePackages}
    '';
  };
in
{
  home.packages = [ mopidyEnv mpc_cli ];

  xdg.configFile."mopidy/mopidy.conf". source = ./mopidy.conf;

  systemd.user.services = {
    mopidy = {
      Unit = {
        Description = "Mopidy music server";
        After = [ "network.target" "sound.target" ];

      };

      Service = {
        ExecStart = "${mopidyEnv}/bin/mopidy";
        Restart = "on-failure";
        RestartSec = 3;
      };

      Install.WantedBy = [ "multi-user.target" ];
    };
  };
}
