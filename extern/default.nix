{ inputs }: with inputs;
let
  hmModules = { };
in
{
  modules = [
    home.nixosModules.home-manager
    ci-agent.nixosModules.agent-profile
  ];

  overlays = [
    nix.overlay
    (final: prev: {
      # needed for nix overlay above
      utillinuxMinimal = prev.utillinux;
    })
    nur.overlay
    devshell.overlay
    (final: prev: {
      deploy-rs = deploy.packages.${prev.system}.deploy-rs;
    })
    pkgs.overlay
  ];

  # passed to all nixos modules
  specialArgs = {
    inherit hmModules;

    overrideModulesPath = "${override}/nixos/modules";
    hardware = nixos-hardware.nixosModules;
  };
}
