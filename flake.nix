{
  description = "Coorish Flake";

  inputs = {
    flake-utils = { url = "github:numtide/flake-utils"; };

    nixpkgs = { url = "github:NixOS/nixpkgs/master"; };
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      with nixpkgs.legacyPackages.${system};
      let
        defaultPackageName = "project-manager";

        config = field: groups: {
          COORISH_JIRA_FIELD = field;
          COORISH_LDAP_GROUPS = groups;
        };

        configs = {
          "technical-cordinator" = p:
            p "customfield_23270" "Tech Coordinators";
          "cto-office-representative" = p: p "customfield_22070" "CTO Office";
          "project-manager" = p: p "customfield_13075" "Managers All";
          "team-head" = p: p "customfield_22470" "Production Heads";
          "project-coordinator" = p: p "customfield_12880" "Senior Project Managers All,Production Heads,Production Board,PMO,Managers All";
        };

        package = (name: field: groups:
          haskell.lib.overrideCabal
          (haskellPackages.callCabal2nix "coorish" ./. { }) (drv: rec {
            pname = "coorish-${name}";

            postInstall = ''
              mv $out/bin/coorish $out/bin/${pname}
            '';

            prePatch = lib.concatStrings
              (lib.mapAttrsToList (k: v: "export ${k}=\"${v}\"\n")
                (config field groups));
          }));
      in rec {
        defaultApp = {
          type = "app";
          program = "${defaultPackage}/bin/coorish-${defaultPackageName}";
        };
        defaultPackage = packages.${defaultPackageName};

        packages = builtins.mapAttrs (n: l: l (package n)) configs;

        devShell = (((haskell.lib.addBuildTools defaultPackage [
          haskellPackages.fswatcher
          haskellPackages.apply-refact
          haskellPackages.cabal-fmt

          zlib

          haskell-language-server
          ormolu
          cabal-install
          hlint
          haskellPackages.stan

        ])).envFunc { }).overrideAttrs
          (f: configs.${defaultPackageName} config);
      });
}
