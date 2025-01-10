# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "myocardio";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };

          packageName = "myocardio";

          haskellPackages = pkgs.haskellPackages.override
            {
              overrides = self: super: {
                myocardio = self.callCabal2nix packageName ./.
                  {
                    scotty = haskellPackages.scotty_0_22;
                  };
              };
            };

        in
        {
          packages.${packageName} =
            haskellPackages.myocardio.overrideAttrs (final: prev: {
              postPatch = ''
                sed -i -e 's#staticBasePath = .*#staticBasePath = "${placeholder "out"}/static"#' app/Main.hs
                sed -i -e 's#svgBasePath = .*#svgBasePath = "${placeholder "out"}/svgs"#' app/Main.hs
              '';
              postInstall = ''
                mkdir -p $out/svgs
                cp svgs/{front,back}.svg $out/svgs/
                mkdir -p $out/static
                cp static/* $out/static
              '';
            });

          packages.default = self.packages.${system}.${packageName};

          defaultPackage = self.packages.${system}.default;

          devShells.default =
            haskellPackages.shellFor {
              withHoogle = true;

              nativeBuildInputs = with pkgs; [
                haskellPackages.haskell-language-server # you must build it with your ghc to work
                cabal-install
                ghcid
                haskellPackages.hlint
                haskellPackages.apply-refact
              ];
              packages = hpkgs: [ hpkgs.myocardio ];
            };
          devShell = self.devShells.${system}.default;
        }) // {
      nixosModule = { pkgs, config, lib, ... }:
        {
          options.services.myocardio3 = {
            enable = lib.mkEnableOption "enable myocardio3";
          };

          config = lib.mkIf config.services.myocardio3.enable {
            systemd.services.myocardio3 = let pkg = self.packages.${pkgs.system}.default; in {
              enable = true;
              serviceConfig = {
                Type = "simple";
                User = "pmidden";
                ExecStart = "${pkg}/bin/myocardio-exe";
                Restart = "always";
              };
              wantedBy = [ "multi-user.target" ];
              after = [ "network.target" ];
            };

          };
        };
    };
}
