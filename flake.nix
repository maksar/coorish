{
  description = "Coorish Flake";

  inputs = {
    flake-utils = { url = "github:numtide/flake-utils"; };

    pre-commit-hooks = { url = "github:cachix/pre-commit-hooks.nix"; };

    nixpkgs = { url = "github:NixOS/nixpkgs/master"; };
  };

  outputs = { self, flake-utils, pre-commit-hooks, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      with nixpkgs.legacyPackages.${system};
      let
        defaultPackageName = "business-analysts";

        config = field: groups: {
          COORISH_JIRA_FIELD = field;
          COORISH_LDAP_GROUPS = groups;
        };

        configs = {
          "technical-cordinator" = p: p "customfield_23270" "Tech Coordinators";
          "cto-office-representative" = p: p "customfield_22070" "CTO Office";
          "project-manager" = p: p "customfield_13075" "Managers All";
          "team-head" = p: p "customfield_22470" "Production Heads";
          "business-analysts" = p: p "customfield_23271" "Business Analysts All";
          "project-coordinator" = p:
            p "customfield_12880"
              "Senior Project Managers All,Production Heads,Production Board,PMO,Managers All";
        };

        package = (name: field: groups:
          haskell.lib.overrideCabal
            (haskellPackages.callCabal2nix "coorish" ./. { })
            (drv: rec {
              pname = "coorish-${name}";

              postInstall = ''
                mv $out/bin/coorish $out/bin/${pname}
              '';

              prePatch = lib.concatStrings (lib.mapAttrsToList
                (k: v: ''
                  export ${k}="${v}"
                '')
                (config field groups));
            }));
        cabal-fmt = haskellPackages.cabal-fmt;
      in
      rec {
        defaultApp = {
          type = "app";
          program = "${defaultPackage}/bin/coorish-${defaultPackageName}";
        };
        defaultPackage = packages.${defaultPackageName};

        packages = builtins.mapAttrs (n: l: l (package n)) configs;

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint = { enable = true; };
              ormolu = { enable = true; };
              nixpkgs-fmt = { enable = true; };
              cabal = {
                enable = true;
                name = "cabal-fmt";
                entry = "${cabal-fmt}/bin/cabal-fmt --inplace";
                files = "\\.cabal$";
              };
            };
          };
        };

        devShell = ((
          (haskell.lib.addBuildTools defaultPackage [
            haskellPackages.fswatcher
            haskellPackages.apply-refact

            zlib

            haskell-language-server
            haskellPackages.cabal-fmt
            pre-commit-hooks.checks.${system}.ormolu
            pre-commit-hooks.checks.${system}.hlint
            pre-commit-hooks.checks.${system}.nixpkgs-fmt
            cabal-install
            haskellPackages.stan
          ])
        ).envFunc { }).overrideAttrs (f:
          (configs.${defaultPackageName} config) // {
            inherit (self.checks.${system}.pre-commit-check) shellHook;
          }
        );
      });
}
