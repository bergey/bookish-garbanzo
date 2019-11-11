{ compiler ? "default", doBenchmark ? false }:

let
    nixpkgs =
        let snapshot = builtins.fromJSON (builtins.readFile ./nixpkgs-snapshot.json);
        inherit (snapshot) owner repo rev;
        in builtins.fetchTarball {
            inherit (snapshot) sha256;
            url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
            };
    pkgs = import nixpkgs { };

    f = import ./default.nix;

    haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

    variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

    drv = variant (haskellPackages.callPackage f {});

in

    if !pkgs.lib.inNixShell then pkgs.haskell.lib.dontCheck drv else
    pkgs.stdenv.mkDerivation {
        name = "haskell-env";
        buildInputs = drv.env.nativeBuildInputs ++ [
        haskellPackages.cabal-install
        pkgs.ghcid
        pkgs.stylish-haskell
        ];
    }
