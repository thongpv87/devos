{ extern
, home
, lib
, nixos
, override
, pkgs
, self
, system
, ...
}:
let
  inherit (lib) dev;

  suites = import ../suites { inherit lib; };

  modules =
    let
      core = ../profiles/core;
      modOverrides = { config, overrideModulesPath, ... }:
        let
          overrides = import ../overrides;
          inherit (overrides) modules disabledModules;
        in
        {
          disabledModules = modules ++ disabledModules;
          imports = map
            (path: "${overrideModulesPath}/${path}")
            modules;
        };


      global =
        let
          experimentalFeatures = [
            "flakes"
            "nix-command"
            "ca-references"
            "ca-derivations"
          ];
        in
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;

          hardware.enableRedistributableFirmware = lib.mkDefault true;

          nix.nixPath = [
            "nixpkgs=${nixos}"
            "nixos-config=${self}/compat/nixos"
            "home-manager=${home}"
          ];

          nixpkgs = { inherit pkgs; };

          nix.registry = {
            devos.flake = self;
            nixos.flake = nixos;
            override.flake = override;
          };

          nix.extraOptions = ''
            --experimental-features = ${lib.concatStringsSep " "
              experimentalFeatures
            }
          '';

          system.configurationRevision = lib.mkIf (self ? rev) self.rev;
        };

      # Everything in `./modules/list.nix`.
      flakeModules =
        builtins.attrValues self.nixosModules;

    in
    flakeModules ++ [
      core
      global
      modOverrides
    ] ++ extern.modules;

  specialArgs = extern.specialArgs // { inherit suites; };

  mkHostConfig = hostName:
    let
      local = {
        require = [
          "${toString ./.}/${hostName}.nix"
        ];

        networking = { inherit hostName; };

        _module.args = {
          inherit self;
        };
      };
    in
    dev.os.devosSystem {
      inherit system specialArgs;

      modules = modules ++ [
        local
        {
          lib = { inherit specialArgs; };
          lib.testModule = {
            imports = modules;
          };
        }
      ];
    };

  hosts = dev.os.recImport
    {
      dir = ./.;
      _import = mkHostConfig;
    };
in
hosts
