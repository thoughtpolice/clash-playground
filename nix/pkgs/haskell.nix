{ compiler ? null
}:

let
  dontCheckPackages =
    [ "clash-prelude" # spurious doctest failures
    ];

  doJailbreakPackages =
    [
    ];

  dontHaddockPackages =
    [
    ];
in
{
  makeEnv = super: k:
    let self = super.pkgs;
    in self.haskell.packages."${compiler}".ghcWithHoogle k;

  packageOverrides = pkgs: rec {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        "${compiler}" =
          let
            generatedOverrides = haskellPackagesNew: haskellPackagesOld:
              let toPackage = file: _: {
                    name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;
                    value = haskellPackagesNew.callPackage (./. + "/hs/${file}") { };
                  };
              in pkgs.lib.mapAttrs' toPackage (builtins.readDir ./hs);

            makeOverrides =
              function: names: haskellPackagesNew: haskellPackagesOld:
              let toPackage = name: {
                    inherit name;
                    value = function haskellPackagesOld.${name};
                  };
              in builtins.listToAttrs (map toPackage names);

            composeExtensionsList =
              pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

            # More exotic overrides go here
            manualOverrides = haskellPackagesNew: haskellPackagesOld: {};
          in
            pkgs.haskell.packages."${compiler}".override {
              overrides = composeExtensionsList [
                generatedOverrides
                (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages  )
                (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
                (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
                manualOverrides
              ];
            };
      };
    };
  };
}
