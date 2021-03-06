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
        defaultPackageName = "technical-cordinator";

        config = field: groups: {
          COORISH_JIRA_FIELD = field;
          COORISH_LDAP_GROUPS = groups;
        };

        configs = {
          "technical-cordinator" = p:
            p "Technical Coordinator" "Tech Coordinators";
          "cto-office-representative" = p:
            p "CTO Office Representative" "CTO Office";
          "project-manager" = p: p "Project manager" "Managers All";
          "team-head" = p: p "Team Head" "Production Heads";
          "business-analysts" = p: p "Business Analyst" "Business Analysts All";
          "project-coordinator" = p:
            p "Project Coordinator"
              "Senior Project Managers All,Production Heads,Production Board,PMO,Managers All";
          "mobile-project-coordinator" = p:
            p "Mobile Project Coordinator"
              "Senior Project Managers All,Production Heads,Production Board,PMO,Managers All";
          "support-project-coordinator" = p:
            p "CS Project Coordinator"
              "Senior Project Managers All,Production Heads,Production Board,PMO,Managers All";
          "account-manager" = p:
            p "Account manager"
              "Account.Managers,Senior Project Managers All,Production Heads,Production Board,PMO";
          "salesperson" = p:
            p "Salesperson"
              "Sales,Production Heads,Production Board,RFX & Business Development,Departments Managers";
        };

        basePackage = haskellPackages.callCabal2nix "coorish" ./coorish { };
        basePackageConsole = haskellPackages.callCabal2nix "console" ./console { coorish = basePackage; };
        basePackageServer = haskellPackages.callCabal2nix "server" ./server { coorish = basePackage; };

        package = (name: field: groups:
          basePackageConsole.overrideDerivation (drv: {
            pname = "coorish-${name}";
            buildInputs = drv.buildInputs or [ ] ++ [ pkgs.makeWrapper ];
            postInstall = ''
              mv $out/bin/coorish-console $out/bin/coorish-${name}
              wrapProgram $out/bin/coorish-${name} --set COORISH_JIRA_FIELD "${field}" --set COORISH_LDAP_GROUPS "${groups}"
            '';
          }));

        flatConfig = (builtins.concatStringsSep ";"
          (map (f: f (a: b: "${a}=${b}")) (lib.attrValues configs)));

        server = basePackageServer.overrideDerivation (drv: {
          pname = "coorish-server";
          buildInputs = drv.buildInputs or [ ] ++ [ pkgs.makeWrapper ];
          postInstall = ''
            wrapProgram $out/bin/coorish-server --set COORISH_SERVER_CONFIG "${flatConfig}"
          '';
        });

        cabal-fmt = haskellPackages.cabal-fmt;
      in
      rec {
        defaultApp = {
          type = "app";
          program = "${configs.${defaultPackageName} (package defaultPackageName)}/bin/coorish-${defaultPackageName}";
        };

        defaultPackage = server;

        packages = {
          server = server;
        } // (builtins.mapAttrs (n: l: l (package n)) configs);

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
            COORISH_SERVER_CONFIG = flatConfig;
          });
      });
}
