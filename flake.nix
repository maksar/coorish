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
        defaultPackageName = "Project manager";

        config = field: groups: {
          COORISH_JIRA_FIELD = field;
          COORISH_LDAP_GROUPS = groups;
        };

        configs = {
          "Technical Coordinator" = p:
            p "customfield_23270" "Tech Coordinators";
          "CTO Office Representative" = p: p "customfield_22070" "CTO Office";
          "Project manager" = p: p "customfield_13075" "Managers All";
          "Team Head" = p: p "customfield_22470" "Production Heads";
          "Project Coordinator" = p: p "customfield_12880" "Senior Project Managers All,Production Heads,Production Board,PMO,Managers All";
        };

        package = (field: groups:
          haskell.lib.overrideCabal
          (haskellPackages.callCabal2nix "coorish" ./. { }) (drv: {
            prePatch = lib.concatStrings
              (lib.mapAttrsToList (k: v: "export ${k}=\"${v}\"\n")
                (config field groups));
          }));
      in rec {
        defaultApp = {
          type = "app";
          program = "${defaultPackage}/bin/coorish";
        };
        defaultPackage = packages.${defaultPackageName};

        packages = builtins.mapAttrs (n: l: l package) configs;

        devShell = (((haskell.lib.addBuildTools defaultPackage [
          haskellPackages.fswatcher
          haskellPackages.apply-refact
          haskellPackages.cabal-fmt

          zlib

          haskell-language-server
          ormolu
          cabal-install
          hlint

        ])).envFunc { }).overrideAttrs
          (f: configs.${defaultPackageName} config);
      });
}
