{ compiler ? null
}:

let
  dontCheckPackages =
    [ "ghc-typelits-knownnat"
      "clash-prelude"
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
            manualOverrides = haskellPackagesNew: haskellPackagesOld: {
              shake      = haskellPackagesOld.shake_0_16;

              # Needed for clash-prelude and clash-compiler
              th-desugar = haskellPackagesOld.th-desugar_1_7;
              singletons = haskellPackagesOld.singletons_2_3_1;
            };
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
