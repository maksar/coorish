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
        defaultPackageName = "salesperson";

        config = field: groups: {
          COORISH_JIRA_FIELD = field;
          COORISH_LDAP_GROUPS = groups;
        };

        configs = {
          "technical-cordinator" = p: p "Technical Coordinator" "Tech Coordinators";
          "cto-office-representative" = p: p "CTO Office Representative" "CTO Office";
          "project-manager" = p: p "Project manager" "Managers All";
          "team-head" = p: p "Team Head" "Production Heads";
          "business-analysts" = p: p "Business Analyst" "Business Analysts All";
          "project-coordinator" = p: p "Project Coordinator" "Senior Project Managers All,Production Heads,Production Board,PMO,Managers All";
          "mobile-project-coordinator" = p: p "Mobile Project Coordinator" "Senior Project Managers All,Production Heads,Production Board,PMO,Managers All";
          "support-project-coordinator" = p: p "CS Project Coordinator" "Senior Project Managers All,Production Heads,Production Board,PMO,Managers All";
          "account-manager" = p: p "Account manager" "Account.Managers,Senior Project Managers All,Production Heads,Production Board,PMO";
          "salesperson" = p: p "Salesperson" "Sales,Production Heads,Production Board,RFX & Business Development,Departments Managers";
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
